
# Cote-Derive

Procedural macro implementation for cote.

# Example

```rust
fn wrap_handler<'a, V, H, S, Args>(mut handler: H) -> impl FnMut(u64, &S) -> Result<(), String> + 'a
where
    V: 'a,
    S: ExtractInnerA<'a, V> + 'a,
    H: Handler<S, Args, Error = String, Output = ()> + 'a,
    Args: Extract<'a, V, S, Output<'a> = Args>,
{
    Box::new(move |uid: u64, set: &S| {
        let args = Args::extract(uid, set).map_err(|v| v.into())?;
        handler.handle(uid, set, args)
    })
}

#[derive(Debug)]
pub struct Widget<'a, V> {
    val: &'a V,
}

impl<'a, V> ExtractInnerA<'a, V> for Widget<'a, V> {
    fn inner(&self) -> &'a V {
        self.val
    }
}

#[derive(Debug)]
pub struct WidgetRef<'a, T: 'a>(&'a T);

impl<'a, V> Extract<'a, V, Widget<'a, V>> for WidgetRef<'a, V> {
    type Output<'b> = WidgetRef<'b, V> where V:'b;

    type Error = String;

    fn extract(_uid: u64, set: &Widget<'a, V>) -> Result<Self::Output<'a>, Self::Error> {
        Ok(WidgetRef(set.inner()))
    }
}

impl<'a> Extract<'a, i64, Widget<'a, i64>> for &'a i64 {
    type Output<'b> = &'b i64
        where
        i64: 'b;

    type Error = String;

    fn extract(_uid: u64, set: &Widget<'a, i64>) -> Result<Self::Output<'a>, Self::Error> {
        Ok(set.inner())
    }
}

impl<'a> Extract<'a, i64, Widget<'a, i64>> for String {
    type Output<'b> = String
        where
        i64: 'b;

    type Error = String;

    fn extract(_uid: u64, set: &Widget<'a, i64>) -> Result<Self::Output<'a>, Self::Error> {
        Ok(set.inner().to_string())
    }
}

extract::extract!(err: String, Extract { uid: u64, set: &A<V> });
extract::handler!(err: String, Handler { uid: u64, set: &A<V> });

fn main() {
    fn annoy1<'a>(uid: u64, w: &Widget<'a, i64>, val: &'a i64) -> Result<(), String> {
        println!("---> in annoy1: uid = {}, widget = {:?}, {}", uid, w, val);
        Ok(())
    }

    fn annoy2(uid: u64, w: &Widget<'_, i64>, val: String) -> Result<(), String> {
        println!("---> in annoy2: uid = {}, widget = {:?}, {}", uid, w, val);
        Ok(())
    }

    fn annoy3<'a>(uid: u64, w: &Widget<'a, i64>, val: WidgetRef<'a, i64>) -> Result<(), String> {
        println!("---> in annoy3: uid = {}, widget = {:?}, {}", uid, w, val.0);
        Ok(())
    }

    type HandlerT<'a> = Box<dyn FnMut(u64, &Widget<'a, i64>) -> Result<(), String> + 'a>;

    let value = Widget { val: &42i64 };
    let handler: Vec<HandlerT<'_>> = vec![
        Box::new(wrap_handler(annoy1)),
        Box::new(wrap_handler(annoy2)),
        Box::new(wrap_handler(annoy3)),
    ];

    for mut h in handler {
        h(0, &value).unwrap();
    }
}
```