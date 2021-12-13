module Win = struct
  type t =
    | NilWin
    | Win of {
     title: string;
     visible: bool;
     child: t;
    }

  type value =
    | Title of string
    | Visible of bool
    | Self of t
    | ChildDiff of value

  let rec update value = function
    | NilWin -> NilWin
    | Win data -> (match value with
      | Title title -> Win { data with title }
      | Visible visible -> Win { data with visible }
      | Self win -> win
      | ChildDiff value -> Win { data with child = update value data.child }
    )
end

module UpdWin = MakeUpd(Win)

(* usage *)

let a = Win.(Win {
  title="main";
  visible=true;
  child=Win {
    title="new task popup";
    visible=true;
    child=NilWin;
  };
})

let update_title title win = (win, Win.[Title title])
let update_child_title child_title win = (win, Win.[ChildDiff (Title child_title)])

let test = UpdWin.(
  return a
  >>= (update_title "new title")
  >>= (update_child_title "new chil")
)

