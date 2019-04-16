let f a=
 let g(s:string)=
  let rec f a (c,s)=function
   |h::t when h=s->f a (c+1,s) t
   |h::t->f ((c,s)::a) (1,h) t
   |_->(c,s)::a
  let x(c,s)=string c+string s
  let h::t=Seq.toList<|s.ToCharArray()
  f [] (1,h) t|>Seq.rev|>Seq.map x|>Seq.reduce(+)
 g(g(g(a)))|>Seq.sumBy(string>>int)