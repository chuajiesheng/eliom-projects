type shape = Lines | Rectangle | Circle;;
type point = (int * int);;
type drawing =
  Lines of (point * point)
| Rectangle of (point * point)
| Circle of (point * int);;

let pts1:point = (1, 2);;
let pts2:point = (3, 4);;
let draw1 = Lines (pts1, pts2);;
