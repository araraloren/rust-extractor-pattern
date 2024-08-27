use proc_macro2::Literal;
use proc_macro2::Span;
use quote::quote;
use syn::braced;
use syn::parse::Parse;
use syn::parse_macro_input;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::Ident;
use syn::Token;

// fn error(spanned: impl Spanned, msg: impl Into<String>) -> syn::Error {
//     syn::Error::new(spanned.span(), msg.into())
// }

#[derive(Debug)]
enum Type {
    Normal(Ident),

    Generics(Ident),

    Inner(Ident, Ident),
}

impl Parse for Type {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ident = input.parse::<Ident>()?;

        Ok(match input.parse::<Token![<]>() {
            Ok(_) => match input.parse::<Ident>() {
                Ok(inner) => {
                    let _ = input.parse::<Token![>]>()?;

                    Type::Inner(ident, inner)
                }
                Err(_) => {
                    let _ = input.parse::<Token![>]>()?;

                    Type::Generics(ident)
                }
            },
            Err(_) => Type::Normal(ident),
        })
    }
}

#[derive(Debug)]
struct Argument {
    ident: Ident,
    ty: Type,
    reference: bool,
}

impl Parse for Argument {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ident = input.parse::<Ident>()?;
        let _ = input.parse::<Token![:]>()?;
        let is_reference = input.parse::<Token![&]>().is_ok();
        let ty = input.parse::<Type>()?;

        Ok(Self {
            ident,
            ty,
            reference: is_reference,
        })
    }
}

impl Argument {
    pub fn gen_generics_ty(&self) -> Option<&Ident> {
        match &self.ty {
            Type::Generics(ty) => Some(ty),
            Type::Inner(ty, _) => Some(ty),
            Type::Normal(_) => None,
        }
    }

    pub fn gen_where_ty(&self) -> Option<&Ident> {
        match &self.ty {
            Type::Inner(ty, _) => Some(ty),
            _ => None,
        }
    }

    pub fn gen_where_bound_ty(&self) -> Option<&Ident> {
        match &self.ty {
            Type::Inner(_, ty) => Some(ty),
            _ => None,
        }
    }

    pub fn gen_extract_arg_names(&self) -> &Ident {
        &self.ident
    }

    pub fn gen_extract_args(&self) -> proc_macro2::TokenStream {
        let ty = match &self.ty {
            Type::Normal(ty) | Type::Generics(ty) | Type::Inner(ty, _) => {
                if self.reference {
                    quote! {
                        &#ty
                    }
                } else {
                    quote! {
                        #ty
                    }
                }
            }
        };
        let ident = &self.ident;

        quote! {
            #ident: #ty
        }
    }

    pub fn gen_function_tys(&self) -> proc_macro2::TokenStream {
        match &self.ty {
            Type::Normal(ty) | Type::Generics(ty) | Type::Inner(ty, _) => {
                if self.reference {
                    quote! {
                        &#ty
                    }
                } else {
                    quote! {
                        #ty
                    }
                }
            }
        }
    }
}

#[derive(Debug)]
struct ProcMacroArgument {
    ident: Ident,
    arguments: Punctuated<Argument, Token![,]>,
    err: Ident,
    len: usize,
}

impl Parse for ProcMacroArgument {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {

        let mut ident = input.parse::<Ident>()?;
        let err = if ident == "err" {
            let _ = input.parse::<Token![:]>()?;
            let err = Some(input.parse::<Ident>()?);
            let _ = input.parse::<Token![,]>()?;

            ident = input.parse::<Ident>()?;
            err
        }
        else {None};
        let len = if ident == "len" {
            let _ = input.parse::<Token![:]>()?;
            let len =Some(input.parse::<Literal>()?.to_string().parse::<usize>().unwrap());
            let _ = input.parse::<Token![,]>()?;

            ident = input.parse::<Ident>()?;
            len
        }
        else {None};

        let content;

        braced!(content in input);

        Ok(Self {
            err: err.unwrap_or(Ident::new("String", ident.span())),
            len: len.unwrap_or(18),
            ident,
            arguments: content.parse_terminated(Argument::parse, Token![,])?,
        })
    }
}

fn generate_extract(ident: Ident, args: Vec<Argument>, err: Ident, len: usize) -> syn::Result<proc_macro2::TokenStream> {
    let generics_tys: Vec<_> = args.iter().filter_map(|v| v.gen_generics_ty()).collect();
    let where_tys: Vec<_> = args.iter().filter_map(|v| v.gen_where_ty()).collect();
    let where_bonud_tys: Vec<_> = args.iter().filter_map(|v| v.gen_where_bound_ty()).collect();
    let inner_trait_tys: Vec<_> = where_tys
        .iter()
        .map(|v| Ident::new(&format!("ExtractInner{}", v), v.span()))
        .collect();
    let extract_args = args
        .iter()
        .map(|v| v.gen_extract_args())
        .collect::<Vec<_>>();
    let extract_arg_names: Vec<_> = args.iter().map(|v|v.gen_extract_arg_names()).collect();
    let mut inner_trait_def = vec![];

    // define inner trait for extract
    for ident in inner_trait_tys.iter() {
        inner_trait_def.push(quote! {
            pub trait #ident<'a, V> {
                fn inner(&self) -> &'a V;
            }
        });
    }

    let extract_trait_def = quote! {
        pub trait #ident<'a, #(#where_bonud_tys,)* #(#generics_tys,)*>
        where
        Self: Sized,
        #(
            #where_tys: #inner_trait_tys<'a, #where_bonud_tys>,
        )*
            {
            type Output<'b> where #(#where_bonud_tys: 'b),*;
            type Error: Into<#err>;

            fn extract(#(#extract_args),*) -> std::result::Result<Self::Output<'a>, Self::Error>;
        }
    };
    let extract_impl_tys: Vec<_> = (0..len).map(|v|format!("ET{v}")).map(|v|Ident::new(&v, Span::call_site())).collect();
    let mut extract_impl_def = vec![];
    let extract_call_args = quote!{ #(#extract_arg_names,)* };

    for len in 0..=extract_impl_tys.len() {
        let slice = &extract_impl_tys[0..len];
        let impl_tys = if len == 0 {
            None
        } else {
            Some(quote! { #(#slice,)* })
        };
        let target_ty = if len == 0 {
            quote! {()}
        } else {
            quote! { (#(#slice,)*) }
        };
        let where_claused = if len == 0 {
            if where_tys.is_empty() {
                quote! {}
            }else {
                quote! {
                    where #(
                        #where_tys: #inner_trait_tys<'a, #where_bonud_tys>,
                    )*
                }
            }
        }
        else {
            let extract_bound = quote! { 'a, #(#where_bonud_tys,)* #(#generics_tys,)* };
            quote! {
                where #(#slice: #ident<#extract_bound Output<'a> = #slice>,)*
                #(
                    #where_tys: #inner_trait_tys<'a, #where_bonud_tys>,
                )*
        }
        };
        let output = if len == 0 {
            quote! {Ok(())}
        } else {
            quote! { Ok((#(#slice::extract(#extract_call_args).map_err(|e|e.into())?,)*)) }
        };

        extract_impl_def.push(quote! {
            impl<'a, #(#where_bonud_tys: 'a,)* #(#generics_tys,)* #impl_tys> #ident<'a, #(#where_bonud_tys,)* #(#generics_tys),*> 
                for #target_ty #where_claused
            {
                    type Output<'b> = #target_ty where #(#where_bonud_tys: 'b),*;
                    type Error = #err;

                    #[allow(unused)]
                    fn extract(#(#extract_args),*) -> std::result::Result<Self::Output<'a>, Self::Error> {
                        #output
                    }
                }
            });
    }

    Ok(quote! {
        /// inner trait definitions
        #(#inner_trait_def)*

        /// extract trait definitions
        #extract_trait_def

        /// extract impl definitions
        #(#extract_impl_def)*
    })
}

fn generate_handler(ident: Ident, args: Vec<Argument>, err: Ident, len: usize) -> syn::Result<proc_macro2::TokenStream> {
    let generics_tys: Vec<_> = args.iter().filter_map(|v| v.gen_generics_ty()).collect();
    let function_tys = args
        .iter()
        .map(|v| v.gen_function_tys())
        .collect::<Vec<_>>();
    let handle_args = args
        .iter()
        .map(|v| v.gen_extract_args())
        .collect::<Vec<_>>();
    let handle_arg_names: Vec<_> = args.iter().map(|v|v.gen_extract_arg_names()).collect();

    let handle_trait_def = quote! {
        pub trait #ident<#(#generics_tys,)* Args> {
            type Output;
            type Error: Into<#err>;

            fn handle(&mut self, #(#handle_args,)* args: Args) -> std::result::Result<Self::Output, Self::Error>;
        }
    };
    let handler_impl_tys: Vec<_> = (0..len).map(|v|format!("HT{v}")).map(|v|Ident::new(&v, Span::call_site())).collect();
    let mut extract_impl_def = vec![];
    let extract_call_args = quote!{ #(#handle_arg_names,)* };

    for len in 0..=handler_impl_tys.len() {
        let slice = &handler_impl_tys[0..len];
        let impl_tys = if len == 0 {
            None
        } else {
            Some(quote! { #(#slice,)* })
        };
        let args_ty = if len == 0 {
            quote! {()}
        } else {
            quote! { (#(#slice,)*) }
        };
        let extracted_tys = if len == 0 {
            quote! {()}
        } else {
            quote! { #(#slice,)* }
        };


        extract_impl_def.push(quote! {
            impl<#(#generics_tys,)* #impl_tys Func, Out, Err> #ident<#(#generics_tys,)* #args_ty> for Func
                where 
                Err: Into<#err>,
                Func: FnMut(#(#function_tys,)* #extracted_tys) -> std::result::Result<Out, Err>,
            {
                    type Output = Out;
                    type Error = Err;

                    #[inline]
                    #[allow(non_snake_case)]
                    fn handle(&mut self, #(#handle_args,)* #args_ty: #args_ty) -> std::result::Result<Self::Output, Self::Error> {
                        (self)(#extract_call_args #extracted_tys)
                    }
                }
            });
    }

    Ok(quote! {
        /// extract trait definitions
        #handle_trait_def

        /// extract impl definitions
        #(#extract_impl_def)*
    })
}

/// extract!(err: Error?, len: 18?, Extract { ident: type, ident: type<inner_type>, ident: type<> })
#[proc_macro]
pub fn extract(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let arg: ProcMacroArgument = parse_macro_input!(input);
    let def_args: Vec<_> = arg.arguments.into_iter().collect();
    let ts = generate_extract(arg.ident, def_args, arg.err, arg.len).unwrap_or_else(syn::Error::into_compile_error);

    quote! {
        #ts
    }
    .into()
}

/// handler!(err: Error?, len: 18?, Handler { ident: type, ident: type<inner_type>, ident: type<> })
#[proc_macro]
pub fn handler(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let arg: ProcMacroArgument = parse_macro_input!(input);
    let def_args: Vec<_> = arg.arguments.into_iter().collect();
    let ts = generate_handler(arg.ident, def_args, arg.err, arg.len).unwrap_or_else(syn::Error::into_compile_error);

    quote! {
        #ts
    }
    .into()
}