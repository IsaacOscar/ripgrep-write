#![allow(unused_macros)]
/// Like assert_eq, but nicer output for long strings.
#[cfg(test)]
#[macro_export]
macro_rules! assert_eq_printed {
    ($expected:expr, $got:expr) => {
        let expected = &*$expected;
        let got = &*$got;
        if expected != got {
            panic!("
printed outputs differ!

expected:
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

got:
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
", expected, got);
        }
    }
}

/// [`impl_deref!`] allows conveniently implemting the [`std::ops::Deref`] and [`std::ops::DerefMut`] traits, and defining an `into_inner` method.
/// For example, if we have:
/// ```ignore
/// # trait Trait1{} trait Trait2{} struct ToTy<T> { t: T }
/// struct MyTy<T: Trait1 + Trait2> {
///     inner: Box<ToTy<T>>
/// }
/// ```
/// Then one can write
/// ```ignore
/// # #[macro_use] extern crate grep_printer_write;
/// # trait Trait1{} trait Trait2{} struct ToTy<T> { t: T }
/// # struct MyTy<T: Trait1 + Trait2> { inner: Box<ToTy<T>> }
/// impl_deref!(pub [T: Trait1 + Trait2] this: MyTy<T> as ToTy<T> = (*this.inner).into(), this.inner.as_ref(), this.as_mut());
/// ```
/// Where
/// * `pub` is an optional visibility qualifier applied to the generated `into_inner` method
/// * `T: Trait1 + Trait2` are optional generic paramater declarations (sadly you have to wrap them in a `[ ... ]` and not `< ... >`)
/// * `this` is an identifier that will hold the `self` value (you can't actually use `self` due to macro hygenie)
/// * `MyTy<T>` as the type to implement the methods/traits for
/// * `ToTy<T>` will be used as the value of `<MyTy<T> as std::ops::Deref>::Target`
/// * `(*this.inner).into()` is the body of the generated `MyTy<T>::into_inner` method
///     (it should have return type `ToTy<T>` where `this` has type `MyTy<T>`)
/// * `this.inner.as_ref()` is the body of the generated `<MyTy<T> as std::ops::Deref>::deref` method
///     (it should have return type `&ToTy<T>` where `this` has type `&MyTy<T>`)
/// * `this.inner.as_mut()` is the body of the generated `<MyTy<T> as std::ops::DerefMut>::deref_mut` method
///     (it should have return type `&mut ToTy<T>` where `this` has type `&mut MyTy<T>`)
/// The last two arguments can be ommited, and they will default to `&expr` and `&mut expr`, where `expr` is the expression used for the body
/// of `into_inner` (this is usefull when expr is a path, e.g. `this.inner`).
/// In other words, the above example will expand to:
/// ```ignore
/// # trait Trait1{} trait Trait2{} struct ToTy<T> { t: T }
/// # struct MyTy<T: Trait1 + Trait2> { inner: Box<ToTy<T>> }
/// impl<T: Trait1 + Trait2> MyTy<T> {
///     pub fn into_inner(self) -> ToTy<T> {
///         let this = self;
///         (*this.inner).into()
///     }
/// }
/// impl<T: Trait1 + Trait2> std::ops::Deref for MyTy<T> {
///     type Target = ToTy<T>;
///     fn deref(&self) -> &Self::Target {
///         let this = self;
///         this.inner.as_ref()
///     }
/// }
/// impl<T: Trait1 + Trait2> std::ops::DerefMut for MyTy<T> {
///     fn deref_mut(&mut self) -> &mut Self::Target {
///         let this = self;
///         this.inner.as_mut()
///     }
/// }
/// ```
#[macro_export]
macro_rules! impl_deref {
    ($vis:vis $([$($gen:tt)+])? $this:ident: $from:ty as $to:ty = $body:expr) => [
        impl_deref!($vis $([$($gen)+])? $this: $from as $to = $body, &$body, &mut $body);
    ];
    ($vis:vis $([$($gen:tt)+])? $this:ident: $from:ty as $to:ty = $owned_body:expr, $ref_body:expr, $mut_body:expr) => [
        impl$(<$($gen)+>)? $from {
            #[allow(dead_code )]
            #[doc = concat!("Consume this `", stringify!($from), "` value and return the inner `", stringify!($to), "`.")]
            $vis fn into_inner(self) -> $to {
                let $this = self;
                $owned_body
            }
        }
        impl $(<$($gen)+>)? ::std::ops::Deref for $from {
            type Target = $to;
            fn deref(&self) -> &Self::Target {
                let $this = self;
                $ref_body
            }
        }
        impl$(<$($gen)+>)? ::std::ops::DerefMut for $from {
            fn deref_mut(&mut self) -> &mut Self::Target {
                let $this = self;
                $mut_body
            }
        }
    ];
}

/// [`forward_impl!`] is an internal macro used for defining [`impl_write!`] and [`impl_write_color!`] below.
/// In particular:
/// ```ignore
/// # #[macro_use] extern crate grep_printer_write;
/// # struct Arg{} struct Ret{} struct Type<T: Trait>{target: T}
/// # trait Trait { fn foo(&mut self, arg: Arg) -> Ret; }
/// # impl<T: Trait> Trait for Type<T> {
/// forward_impl!([Trait] foo([&mut] this, arg: Arg) -> Ret [forward] forward(&mut this.target, arg));
/// # }
/// ```
/// Will
/// * Implement a method `foo(&mut self, arg: Arg) -> Ret`, from the trait `Trait`
/// * `forward(&mut this.target, arg)` is the body of the method (for macro hygenie reasons, one must use the given identifier, `this`, to refer to `self` )
/// * The body will have access to a local `fn forward<T: Trait>(inner: &mut T, arg: Arg) -> Ret`, which simply calls `inner.foo(arg)`
/// * The `[&mut]` in the macro invocation be replaced with `[&]`, or `[]`, which will change the above `&mut`s to `&`, or the empty string.
/// If the method takes no paramaters, you can ommit the `: Arg` part in the above macro invocation, however:
/// * You must still pass the `arg` identifier
/// * The `forward` method will still take too paramaters (the second will have type `()`)
#[macro_export] //#[cfg_attr(doctest, macro_export)]
macro_rules! forward_impl {
    ([$($trait:tt)+] $method:ident([$($ref:tt)*] $this:ident, $arg:ident: $arg_ty:ty) -> $ret_ty:ty [$forward:ident] $body:expr) => [
        #[inline(always)]
        fn $method($($ref)* self, $arg: $arg_ty) -> $ret_ty {
            let $this = self;
            #[inline(always)]
            fn $forward<T: $($trait)+>(inner: $($ref)* T, $arg: $arg_ty) -> $ret_ty { inner.$method($arg) }
            $body
        }
    ];
    ([$($trait:tt)+] $method:ident([$($ref:tt)*] $this:ident, $arg:ident) -> $ret_ty:ty [$forward:ident] $body:expr) => [
        #[inline(always)]
        fn $method($($ref)* self) -> $ret_ty {
            let $this = self;
            let $arg = ();
            #[inline(always)]
            fn $forward<T: $($trait)+>(inner: $($ref)* T, _: ()) -> $ret_ty { inner.$method() }
            $body
        }
    ]
}

/// [`impl_write!`] conveniently implements the `std::io::Write` trait by forwarding to another implementation.
/// In particular:
/// ```ignore
/// # #[macro_use] extern crate grep_printer_write;
/// # trait Trait {} struct MyTy<T>{condition: bool, inner1: Vec<u8>, inner2: std::io::Empty, phantom: T }
/// impl_write!([T: Trait] this: MyTy<T> => [forward, arg] if this.condition { forward(this.inner1, arg) } else { forward(this.inner2, arg) });
/// ```
/// Will
/// * Implement [`std::io::Write`] for `MyTy<T>`, whenever `T` implements Trait.
/// * Within the body of each method, `this` must be used to refer to the value of `self` (you can't actually use `self` due to macro hygenie)
/// * The body of each method will perform `if this.condition { forward(this.inner1, arg) } else { forward(this.inner2, arg) }`
/// * The arguments to each method will be accessible as `arg` (or if no argument, `arg` will simply equal `()`);
/// * The body of each method can call `forward`, passing it any implementation of [`std::io::write`] as the first paramater, and `arg` as the second.
///     (`arg` *must* be passed as localy defined functions can't capture variables, and closures can't have generic paramaters).
/// If the type of the first argument to `forward` is always the same (the types of `this.inner1` and `this.inner2`), you can use the following shorthand:
/// ```ignore
/// # #[macro_use] extern crate grep_printer_write;
/// # trait Trait {} struct MyTy<T>{condition: bool, inner1: Vec<u8>, inner2: Vec<u8>, phantom: T}
/// impl_write!([T: Trait] this: MyTy<T> => if this.condition { this.inner1 } else { this.inner2 });
/// ```
#[macro_export]
macro_rules! impl_write {
    ($([$($gen:tt)+])? $this:ident: $for:ty => $inner:expr) => [
        impl_write!($([$($gen)+])? $this: $for => [forward, arg] forward($inner, arg));
    ];
    ($([$($gen:tt)+])? $this:ident: $for:ty => [$forward:ident, $arg:ident] $body:expr) => [
        // Write needs to be implemented for BufferWriter as it will get wrapped by Ansi or NoColor
        impl $(<$($gen)+>)? ::std::io::Write for $for {
            forward_impl!([::std::io::Write] write([&mut] $this, $arg: &[u8]) -> ::std::io::Result<usize> [$forward] $body);
            forward_impl!([::std::io::Write] flush([&mut] $this, $arg) -> ::std::io::Result<()> [$forward] $body);
            forward_impl!([::std::io::Write] write_all([&mut] $this, $arg: &[u8]) -> ::std::io::Result<()> [$forward] $body);
            forward_impl!([::std::io::Write] write_fmt([&mut] $this, $arg: ::std::fmt::Arguments<'_>) -> ::std::io::Result<()> [$forward] $body);
            // Don't implement as the return type is Self:
            // fn by_ref(&mut self) -> &mut Self where Self: Sized
        }
    ];
}

/// [`impl_write_color!`] is like [`impl_write!`] but it implements [`termcolor::WriteColor`]
/// Additional, you can pass an extra body expression at the end if you need a different body when `self` is `&mut`
macro_rules! impl_write_color {
    ($([$($gen:tt)+])? $this:ident: $for:ty => $inner:expr$(, $inner_mut:expr)?) => [
        impl_write_color!($([$($gen)+])? $this: $for => [forward, arg] forward($inner, arg)$(, forward($inner_mut, arg))?);
    ];
    ($([$($gen:tt)+])? $this:ident: $for:ty => [$forward:ident, $arg:ident] $body:expr) => [
        impl_write_color!($([$($gen)+])? $this: $for => [$forward, $arg] $body, $body);
    ];
    ($([$($gen:tt)+])? $this:ident: $for:ty => [$forward:ident, $arg:ident] $body:expr, $mut_body:expr) => [
        // Write needs to be implemented for BufferWriter as it will get wrapped by Ansi or NoColor
        impl $(<$($gen)+>)? ::termcolor::WriteColor for $for {
            forward_impl!([::termcolor::WriteColor] supports_color([&] $this, $arg) -> bool [$forward] $body);
            forward_impl!([::termcolor::WriteColor] supports_hyperlinks([&] $this, $arg) -> bool [$forward] $body);
            forward_impl!([::termcolor::WriteColor] is_synchronous([&] $this, $arg) -> bool [$forward] $body);
            forward_impl!([::termcolor::WriteColor] set_color([&mut] $this, $arg: &::termcolor::ColorSpec) -> ::std::io::Result<()> [$forward] $mut_body);
            forward_impl!([::termcolor::WriteColor] set_hyperlink([&mut] $this, $arg: &::termcolor::HyperlinkSpec) -> ::std::io::Result<()> [$forward] $mut_body);
            forward_impl!([::termcolor::WriteColor] reset([&mut] $this, $arg) -> ::std::io::Result<()> [$forward] $mut_body);
    }
    ];
}

/*
/// [`impl_write_count!`] is like [`impl_write!`] but it implements [`crate::WriteCount`]
macro_rules! impl_write_count {
    ($([$($gen:tt)+])? $this:ident: $for:ty => $inner:expr) => [
        impl_write_count!($([$($gen)+])? $this: $for => [forward, arg] forward($inner, arg));
    ];
    ($([$($gen:tt)+])? $this:ident: $for:ty => [$forward:ident, $arg:ident] $body:expr) => [
        // Write needs to be implemented for BufferWriter as it will get wrapped by Ansi or NoColor
        impl $(<$($gen)+>)? $crate::WriteCount for $for {
            forward_impl!([$crate::WriteCount] count([&] $this, $arg) -> u64 [$forward] $body);
            forward_impl!([$crate::WriteCount] total_count([&] $this, $arg) -> u64 [$forward] $body);
            forward_impl!([$crate::WriteCount] reset_count([&mut] $this, $arg) -> () [$forward] $body);
    }
    ];
}

/// `match_field!(MyTy obj, field)` is like `obj.field`, except that you don't need to prepend an `&` or `&mut`
/// if `obj` is a `&MyTy<_>` or `&mut MyTy<_>`.
macro_rules! match_field {
    ($ty:path, $obj:expr, $field:ident) => {
        match $obj {
            $ty { $field, .. } => $field,
        }
    };
}
*/
