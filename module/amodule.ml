module List = struct 
    include List 
    let hi() = print_endline "Hi I am extended!"
end

let _ = List.hi()


