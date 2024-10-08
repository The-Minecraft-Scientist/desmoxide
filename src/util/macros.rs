#[macro_export]
macro_rules! bad_token {
    ($s:expr, $t:expr) => {{
        anyhow::bail!("bad token {:?} at {:?}", $t, $s)
    }};
    ($s:expr, $t:expr, $msg:expr) => {{
        anyhow::bail!("bad token {:?} at {:?}: {}", $t, $s, $msg)
    }};
}
#[macro_export]
macro_rules! assert_token_matches {
    ($l:expr, $e:pat) => {{
        let __token = $l.next().context("unexpected EOF")?;
        if !matches!(__token.1, $e) {
            anyhow::bail!(
                "Next token {:?} did not match expected token {:?} at line {} of {}",
                __token.1,
                stringify!($e),
                line!(),
                file!(),
            );
        }
    }};
}
#[macro_export]
macro_rules! compiler_error {
    ($nid:expr, $($msg:tt)+) => {
        anyhow::bail!("Compile error at {:?}:{}", $nid, format!($($msg)+))
    };
}
#[macro_export]
macro_rules! permute {
    ($lhs:pat, $rhs:pat, $($other:pat),*) => {
        ($lhs, $rhs, $($other)*) | ($rhs, $lhs, $($other)*)
    };
}
#[macro_export]
macro_rules! impl_trivial_conversion {
    ($v:ident,$inner:ty) => {
        impl From<$v> for $inner {
            fn from(val: $v) -> $inner {
                val.0
            }
        }
        impl From<$inner> for $v {
            fn from(val: $inner) -> $v {
                $v(val)
            }
        }
    };
}
