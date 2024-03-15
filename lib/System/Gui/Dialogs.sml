
fun dialog_input_show(message: String): String => {
  val str = ""
  java {
    "str = JOptionPane.showInputDialog(null, message);"
  }
  str
}

fun dialog_message_show(message: String): Unit => {
  java {
    "JOptionPane.showMessageDialog(null, message);"
  }
  ()
}

enum DialogChoice = Yes | No | Cancel

fun dialog_yesno_show(message: String): DialogChoice => {
  val result = -1 
  java {
    " result = JOptionPane.showConfirmDialog(null, message);"
  }
  val r = match result {
    0 -> DialogChoice.Yes
    1 -> DialogChoice.No
    _ -> DialogChoice.Cancel
  }
  r as DialogChoice
}

enum DialogIcon = 
  Error
  | Information
  | Warning
  | Question
  | Plain

fun dialog_message_type_code(msg: DialogIcon): Int => match msg {
  DialogIcon.Error -> 0
  DialogIcon.Information -> 1
  DialogIcon.Warning -> 2
  DialogIcon.Question -> 3
  _ -> 4
}

fun dialog_message_with_title_and_icon_show(message: String, title: String, icon: DialogIcon): Unit => {
  val code = dialog_message_type_code(icon)
  java {
    "JOptionPane.showMessageDialog(null, message, title, code);"
  }
  ()
}


