let input = {|intitle:"Index of" phpmyadmin|}

type dork_info = {
    title_keywords : string list;
    body_keywords : string list;
    path_components : string list;
}

type parser = {
    info : dork_info;
    left_to_parse: string;
}


let new_parser (input: string) : parser = {
    info = {
        title_keywords = [];
        body_keywords = [];
        path_components = [];
    };
    left_to_parse = input

}

(* 
   when encountering an operator that does something, parse at that level, THEN
   pass control flow back (return)

   i think i saw a parser that when you hit a grammar rule that encloses other
   stuff you kinda just set a flag then continue. the next iteration will parse
   differently because the flag is different. we are not doing that
 *)

let rec parse (p: parser) : parser =
    if String.length p.left_to_parse = 0 then p else

    let add_title_keyword (title_keyword: string) (info: dork_info) : dork_info =
        { info with title_keywords = info.title_keywords @ [ title_keyword ] }
    in

    let add_body_keyword (body_keyword: string) (info: dork_info) : dork_info =
        { info with body_keywords = info.body_keywords @ [ body_keyword ] }
    in

    let add_path_component (path_component: string) (info: dork_info) : dork_info =
        { info with path_components = info.path_components @ [ path_component ] }
    in

    let get_word (str: string) : (string * string) =
        let next_space_loc = try String.index str ' ' with Not_found -> -1 in
        if next_space_loc = -1 
        then 
            str, ""
        else
            let parsed_string = 
                String.sub str 0 next_space_loc in
            let rest_of_string = 
                String.sub str (next_space_loc + 1) ((String.length str) - 1) in
            (parsed_string, rest_of_string)
    in

    (* 
       pass the start of a word
       if it doesnt start with a quote, it will return the substring until space

       returns (parsed_word, rest_of_string)
     *)
    let parse_keyword (str: string) : (string * string) = 
        if str.[0] != '"' then 
            get_word str
        else
            let next_quote_loc = try String.index str '"' with Not_found -> -1 in
            if next_quote_loc = -1 then 
                get_word str
            else
                let parsed_string = 
                    String.sub str 1 next_quote_loc in
                let rest_of_string =
                    String.sub str (next_quote_loc + 1) ((String.length str) - 1) in
                (parsed_string, rest_of_string)
    in

    if String.starts_with p.left_to_parse ~prefix:"intitle:" then 
        let keyword, rest_of_string = parse_keyword p.left_to_parse in
        let next : parser = {
            info = add_title_keyword keyword p.info;
            left_to_parse = rest_of_string
        } in
        parse next
    else

    if String.starts_with p.left_to_parse ~prefix:"inurl:" then 
        let component, rest_of_string = parse_keyword p.left_to_parse in
        let next : parser = {
            info = add_path_component component p.info;
            left_to_parse = rest_of_string
        } in
        parse next
    else

    let keyword, rest_of_string = parse_keyword p.left_to_parse in
    let next : parser = {
        info = add_body_keyword keyword p.info;
        left_to_parse = rest_of_string
    } in
    parse next

(* TODO

    let craft (p: parser) (c: crafter) : crafter

*)


let () = 
    let parser = new_parser input in
    let parsed = parse parser in

    Printf.printf "Path Components:\n";
    List.iter print_endline parsed.info.path_components;

    Printf.printf "Title Keywords:\n";
    List.iter print_endline parsed.info.title_keywords;

    Printf.printf "Body Keywords:\n";
    List.iter print_endline parsed.info.body_keywords;
    Printf.printf "\n";
