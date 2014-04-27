struct s<t> { a: t }
struct s { a: t, b: t2, c: t3, }
struct s { #[x] #[y] a: t, #[z] b: t2 }
struct s ();
struct s (t);
struct s<t, t2, t3> (t, t2, t3,);
struct s (#[x] #[y] t);
struct s (#[x] #[y] t, #[z] t2);
struct s;
struct s %
