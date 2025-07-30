#[macro_export]
/// DOCUMENTATION
macro_rules! COVERAGE {
    (= $n:literal) => {
        #[cfg(windows)]
        if true {
            // Needed to surpress dead code warnings
            panic!("COVERAGE>>>>>>>>>>>>>>>>>>>>>>\n{}", $n)
        }
    };
    (== $n:literal) => {};
    ($n:literal) => {
        if true {
            // Needed to surpress dead code warnings
            panic!("COVERAGE>>>>>>>>>>>>>>>>>>>>>>\n{}", $n)
        }
    };
}

/// Like assert_eq, but nicer output for long strings.
#[cfg(test)]
#[macro_export]
macro_rules! assert_eq_printed {
    ($expected:expr, $got:expr, $($tt:tt)*) => {
        let expected = &*$expected;
        let got = &*$got;
        let label = format!($($tt)*);
        if expected != got {
            panic!("
printed outputs differ! (label: {})

expected:
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

got:
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
", label, expected, got);
        }
    }
}
