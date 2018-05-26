open Opium.Std
open Opium.App
open Batteries.Hashtbl
open Batteries.List

type person = {
  name: string;
  age: int;
}

type movie = {
  title: string;
}

let hashtbl_values h =
  BatList.of_enum (BatHashtbl.values h) ;;

let json_of_person { name ; age } =
  let open Ezjsonm in
  dict [ "name", (string name)
       ; "age", (int age) ]

let json_of_movie m =
  let open Ezjsonm in
  dict [
    "title", (string m.title);
  ]

let json_of_movie_list l =
  Ezjsonm.list json_of_movie l

let movie_of_json j =
  let open Ezjsonm in
  {
    title = find (value j) ["title"] |> get_string
  }

let movies = Hashtbl.create 128 ;;

[
  { title = "Foo" } ;
  { title = "Bar" } ;
  { title = "Baz" }
] 
|> List.iteri (fun i m -> Hashtbl.add movies i m) ;;

let _next_movie_id = ref (Hashtbl.length movies) ;;

let next_movie_id() =
  let id = !_next_movie_id in
  let _ = _next_movie_id := !_next_movie_id + 1 in
  id

let catch_not_found =
  let filter handler req =
    try handler req
    with Not_found -> `String "" |> respond' ~code:`Not_found
  in
  Rock.Middleware.create ~filter ~name:"catch_not_found"

let print_param = put "/hello/:name" begin fun req ->
  `String ("Hello " ^ param req "name") |> respond'
end

let print_person = get "/person/:name/:age" begin fun req ->
  let person = {
    name = param req "name";
    age = "age" |> param req |> int_of_string;
  } in
  `Json (person |> json_of_person) |> respond'
end

let get_movie = get "/movies/:id" begin fun req ->
  let id = param req "id" |> int_of_string in
  let movie = Hashtbl.find movies id in
  `Json (movie |> json_of_movie) |> respond'
end

let get_all_movies = get "/movies" begin fun req ->
  `Json (json_of_movie_list (hashtbl_values movies)) |> respond'
end

let create_movie = post "/movies" begin fun req ->
(*   let%lwt j = json_of_body_exn req in
  let movie = movie_of_json j in
  let _ = Hashtbl.add movies next_movie_id() movie in *)
  `String "" |> respond'
end

let _ =
  App.empty
  |> print_param
  |> print_person
  |> get_movie
  |> get_all_movies
  |> middleware catch_not_found
  |> App.run_command
