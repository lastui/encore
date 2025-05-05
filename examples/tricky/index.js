//OK
(x=>3);
(x=>({}));
((x)=>3);
((x)=>({}));
(x)=>{};
(x,y,z)=>({});
(x,y) => 3;
x=>3;
[x=>3];
x=>({});
(x=>3)(3);
x=>{};
async function* foo(a, b, ...c) {}
function* foo(a) {}
(function foo() {}())
((x)=>3)(3);
(x,y,z,a,b,c,d)=>3;
(function() {})
({ x: y => 3 });

// ERROR
//function foo(a=1) {}
{ x: y => 3 };
