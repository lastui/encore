//OK
//(x=>3);
//(x=>({}));
//((x)=>3);
//((x)=>({}));
//(x)=>{};
//(x,y,z)=>({});
//(x,y) => 3;
//x=>3;
//[x=>3];
//x=>({});
//{ x: y => 3 };
//({ x: y => 3 });
//x=>{};
//async function* foo(a, b, ...c) {}
//function* foo(a) {}
//(function foo() {}())


// ERROR
//(x=>3)(3);
//((x)=>3)(3);
//(x,y,z,a,b,c,d)=>3;
//function foo(a=1) {}
//(function() {})