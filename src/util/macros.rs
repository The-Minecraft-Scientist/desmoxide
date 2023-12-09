#[macro_export]
macro_rules! bad_token {
    ($s:expr, $t:expr) => {{
        anyhow::bail!("bad token {:?} at {:?}", $t, $s.char_indices())
    }};
    ($s:expr, $t:expr, $msg:expr) => {{
        anyhow::bail!("bad token {:?} at {:?}: {}", $t, $s.char_indices(), $msg)
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
