pub fn t3assert(_: bool) {}
pub fn t3assume(_: bool) {}
pub fn invariant(_: bool) {}
pub fn rand_bool() -> bool { false }
pub fn rand_int<T: From<i8>>() -> T { T::from(0) }
pub fn rand_float<T: From<f32>>() -> T { T::from(0.0) }
pub fn t3drop<T>(_: T) {}
