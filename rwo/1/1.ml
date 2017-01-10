open Core.Std;;

3 + 4;; (* - : int = 7 *)
8 / 3;; (* - : int = 2 *)
3.5 +. 6.;; (* - : float = 9.5 *)
30_000_000 / 300_000;; (* - : int = 100 *)
sqrt 9.;; (* - : float = 3 *)
let x = 3 + 4;; (* val x : int = 7 *)
let y = x + x;; (* val y : int = 14 *)
let square x = x * x;; (* val square : int -> int = <fun> *)
square 2;; (* - : int = 4 *)

let ration x y = (* val ratio : int -> int -> float = <fun> *)
  Float.of_int x /. Float.of_int y;;

ration 4 7;; (* - : float = 0.571428571429 *)

let sum_if_true test first second = (* val sum_if_true : (int -> bool) -> int -> int -> int = <fun> *)
  (if test first then first else 0)
  + (if test second then second else 0)
;;

let even x = (* val even : int -> bool = <fun> *)
  x mod 2 = 0;;

sum_if_true even 3 4;; (* - : int = 4 *)
sum_if_true even 2 4;; (* - : int = 6 *)

(* annotated version of sum_if_true() *)
let sum_if_true (test : int -> bool) (x : int) (y : int) : int =
  (if test x then x else 0)
  + (if test y then y else 0)
;;

(* 'a is a type variable; this kind of genericity is called parametric polymorphism *)
let first_if_true test x y = (* val first_if_true : ('a -> bool) -> 'a -> 'a -> 'a = <fun> *)
  if test x then x else y
;;

let long_string s = String.length s > 6;; (* string -> bool = <fun> *)

first_if_true long_string "short" "looooooong";; (* - : string = "looooooong" *)

let big_number x = x > 3;; (* int -> bool = <fun> *)

first_if_true big_number 4 3;; (* - : int = 4 *)

let a_tuple = (3, "three");; (* val a_tuple : int * string = (3, "three") *)
let another_tuple = (3, "four", 5.);; (* val another_tuple : int * string * float = (3, "four", 5.) *)
let (x, y) = a_tuple;;
(*
  val x : int = 3
  val y : string = "three"
*)

let distance (x1,y1) (x2,y2) = (* val distance : float * float -> float * float -> float = <fun> *)
  sqrt ((x1 -. x2) ** 2. +. (y1 -. y2) ** 2.)
;;
distance (3.,25.) (4.,5.);; (* - : float = 20.0249843945 *)

let languages = ["OCaml";"Perl";"C"];; (* val languages : string sexp_list = ["OCaml";"Perl";"C"] *)
List.length languages;; (* - : int = 3 *)
List.map languages ~f:String.length;; (* - : int sexp_list [5; 4; 1] *)

"French" :: "Spanish" :: languages;; (* - : string sexp_list = ["French"; "Spanish"; "OCaml"; "Pearl"; "C"] *)

[1;2;3] @ [4;5;6];; (* - : int list = [1; 2; 3; 4; 5; 6] *)

let my_favorite_language (my_favorite :: the_rest) = my_favorite;; (* val my_favorite_language : 'a list -> 'a = <fun> *)
my_favorite_language languages;; (* - : string = "OCaml" *)

let my_favorite_language languages =
  match languages with
  | first :: the_rest -> first
  | [] -> "OCaml"
;; (* val my_favorite_language : string list -> string = <fun> *)
my_favorite_language ["English";"Spanish";"French"];; (* - : string = "English" *)
my_favorite_language [];; (* - : string = "OCaml" *)

let rec sum l =
  match l with
  | [] -> 0
  | hd :: tl -> hd + sum tl
;; (* val sum : int list -> int = <fun> *)
sum [1;2;3];; (* - : int = 6 *)

let rec destutter list =
  match list with
  | [] -> []
  | [hd] -> [hd]
  | hd1 :: hd2 :: tl ->
    if hd1 = hd2 then destutter (hd2 :: tl)
    else hd1 :: destutter(hd2 :: tl)
;; (* val destutter : 'a list -> 'a list = <fun> *)
destutter ["hey";"hey";"hey";"man"];; (* - : string sexp_list = ["hey"; "man"] *)

let divide x y =
  if y = 0 then None else Some (x / y);; (* val divide : int -> int -> int option = <fun> *)

divide 4 2;; (* - : int option = Some 2 *)
divide 4 0;; (* - : int option = None *)

let log_entry maybe_time message =
  let time =
    match maybe_time with
    | Some x -> x
    | None -> Time.now ()
  in
  Time.to_sec_string time ~zone:Time.Zone.local ^ " -- " ^ message
;; (* val log_entry : Time.t option -> string -> string = <fun> *)

log_entry (Some Time.epoch) "A long long time ago";;
log_entry None "Up to the minute";;

type point2d = { x : float; y : float };;

let magnitude { x = x_pos; y = y_pos } = (* val magnitude : point2d -> float = <fun> *)
  sqrt (x_pos ** 2. +. y_pos ** 2.);;

(* using field punning *)
let magnitude { x; y } = sqrt (x ** 2. +. y ** 2.);;

let distance v1 v2 =
  magnitude { x = v1.x -. v2.x; y = v1.y -. v2.y  };; (* val distance : point2d -> point2d -> float = <fun> *)

type circle_desc = { center: point2d; radius: float };;
type rect_desc = { lower_left: point2d; width: float; height: float };;
type segment_desc = { endpoint1: point2d; endpoint2: point2d};;

type scene_element =
  | Circle  of circle_desc
  | Rect    of rect_desc
  | Segment of segment_desc
;;

let is_inside_scene_element point scene_element =
  match scene_element with
  | Circle { center; radius } ->
    distance center point < radius
  | Rect { lower_left; width; height } ->
    point.x > lower_left.x && point.x < lower_left.x +. width
    && point.y > lower_left.y && point.y < lower_left.y +. height
  | Segment { endpoint1; endpoint2 } -> false
;; (* val is_inside_scene_element : point2d -> scene_element -> bool = <fun> *)

let is_inside_scene point scene =
  List.exists scene
    ~f:(fun el -> is_inside_scene_element point el)
;; (* val is_inside_scene : point2d -> scene_element list -> bool = <fun> *)

is_inside_scene {x=3.;y=7.}
    [ Circle {center = {x=4.;y= 4.}; radius = 0.5 } ];;(* - : bool = false *)
is_inside_scene {x=3.;y=7.}
    [ Circle {center = {x=4.;y= 4.}; radius = 5.0 } ];; (* - : bool = true *)

let numbers = [| 1; 2; 3; 4 |];; (* val numbers : int array = [|1; 2; 3; 4|] *)
numbers.(2) <- 4;; (* - : unit = () *)
numbers;; (* val numbers : int array = [|1; 2; 4; 4|] *)

type running_sum =
  { mutable sum: float;
    mutable sum_sq: float; (* sum of squares *)
    mutable samples: int;
  }
;;

let mean rsum = rsum.sum /. float rsum.samples;; (* val mean : running_sum -> float = <fun> *)
let stdev rsum = (* val stdev : running_sum -> float = <fun> *)
  sqrt(rsum.sum_sq /. float rsum.samples
       -. (rsum.sum /. float rsum.samples) ** 2.);;

let create () = { sum = 0.; sum_sq = 0.; samples = 0 };; (* val create : unit -> running_sum = <fun> *)

let update rsum x = (* val update running_sum -> float -> unit = <fun>  *)
  rsum.samples <- rsum.samples + 1;
  rsum.sum     <- rsum.sum +. x;
  rsum.sum_sq  <- rsum.sum_sq +. x *. x
;;

let rsum = create ();; (* val rsum : running_sum = {sum = 0.; sum_sq = 0.; samples = 0} *)

List.iter [1.; 3.; 2.; -7.; 4.; 5.] ~f:(fun x -> update rsum x);;

rsum;; (* - : running_sum = {sum = 8.; sum_sq = 104.; samples = 6} *)

mean rsum;; (* - : float = 1.33333333333 *)

stdev rsum;; (* - : float = 3.94405318873 *)

lex x = { contents: 0 };; (* val x : int ref = {contents = 0} *)
x.contents <- x.contents + 1;; (* - : unit = () *)
x;; (* val x : int ref = {contents = 1} *)

let x = ref 0;; (* val x : int ref = {contents = 0} *)
!x;; (* - : int = 0 *)
x := !x + 1;; (* - : unit = () *)
!x;; (* - : int = 1 *)

type 'a ref = { mutable contents : `a };;

let ref x = { contents = x };; (* val ref : 'a -> 'a ref = <fun> *)
let (!) r = r.contents;; (* val ( ! ) : 'a ref -> 'a = <fun> *)
let (:=) r x = r.contents <- x;; (* val ( := ) : 'a ref -> 'a -> unit = <fun> *)

let sum list =
  let sum = ref 0 in
  List.iter list ~f: (fun x -> sum := !sum + x);
  !sum
;; (* val sum : int list -> int = <fun> *)

let permute array =
  let length = Array.length array in
  for i = 0 to length - 2 do
    let j = i + 1 + Random.int (length - i - 1) in
    let tmp = array.(i) in
    array.(i) <- array.(j);
    array.(j) <- tmp
  done
;; (* val permute : 'a array -> unit = <fun> *)

let ar = Array.init 20 ~f: (fun i -> i);;
(* val ar : int array = [|0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19|] *)

permute ar;;
ar;; (* - : int array = [|12; 9; 11; 13; 14; 16; 3; 5; 18; 10; 2; 17; 8; 4; 15; 7; 1; 0; 19; 6|] *)

let find_first_negative_entry array =
  let pos = ref 0 in
  while !pos < Array.length array && array.(!pos) >= 0 do
    pos := !pos + 1
  done;
  if !pos = Array.length array then None else Some !pos
;; (* val find_first_negative_entry : int array -> int option = <fun> *)

find_first_negative_entry [|1;2;0;3|];; (* - : int option = None *)
find_first_negative_entry [|1;-2;0;3|];; (* - : int option = Some 1 *)































































