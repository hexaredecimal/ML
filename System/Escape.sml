
(*
*    =========================== SMLL Standard Libary =====================================
*                   File: System/Io.smll
*                   Created by: Gama Sibusiso
*                   Date: 20-02-2024 
* *)

(* Misc *)
fun clear_formatting(): String => "\033[0m"


(* Text effects *)
fun bold(c: String): String => "\033[1m" + c + clear_formatting()
fun underline(c: String): String => "\033[4m" + c + clear_formatting()
fun block(c: String): String => "\033[7m" + c + clear_formatting()
fun strikethrough(c: String): String => "\033[9m" + c + clear_formatting()
fun double_underline(c: String): String => "\033[21m" + c + clear_formatting()

(* Foreground colors *)
fun fg_black(msg: String): String => "\033[30m" + msg + clear_formatting()
fun fg_red(msg: String): String => "\033[31m" + msg + clear_formatting()
fun fg_green(msg: String): String => "\033[32m" + msg + clear_formatting()
fun fg_yellow(msg: String): String => "\033[33m" + msg + clear_formatting()
fun fg_blue(msg: String): String => "\033[34m" + msg + clear_formatting()
fun fg_magenta(msg: String): String => "\033[35m" + msg + clear_formatting()
fun fg_cyan(msg: String): String => "\033[36m" + msg + clear_formatting()
fun fg_white(msg: String): String => "\033[37m" + msg + clear_formatting()

(* Background ground colors *)
fun bg_black(msg: String): String => "\033[40m" + msg + clear_formatting()
fun bg_red(msg: String): String => "\033[41m" + msg + clear_formatting()
fun bg_green(msg: String): String => "\033[42m" + msg + clear_formatting()
fun bg_yellow(msg: String): String => "\033[43m" + msg + clear_formatting()
fun bg_blue(msg: String): String => "\033[44m" + msg + clear_formatting()
fun bg_magenta(msg: String): String => "\033[45m" + msg + clear_formatting()
fun bg_cyan(msg: String): String => "\033[46m" + msg + clear_formatting()
fun bg_white(msg: String): String => "\033[47m" + msg + clear_formatting()


(* Bright Foreground colors *)
fun bright_fb_black(msg: String): String => "\033[90m" + msg + clear_formatting()
fun bright_fb_red(msg: String): String => "\033[91m" + msg + clear_formatting()
fun bright_fb_green(msg: String): String => "\033[92m" + msg + clear_formatting()
fun bright_fb_yellow(msg: String): String => "\033[93m" + msg + clear_formatting()
fun bright_fb_blue(msg: String): String => "\033[94m" + msg + clear_formatting()
fun bright_fb_magenta(msg: String): String => "\033[95m" + msg + clear_formatting()
fun bright_fb_cyan(msg: String): String => "\033[96m" + msg + clear_formatting()
fun bright_fb_white(msg: String): String => "\033[97m" + msg + clear_formatting()

(* Bright Background ground colors *)
fun bright_bg_black(msg: String): String => "\033[100m" + msg + clear_formatting()
fun bright_bg_red(msg: String): String => "\033[101m" + msg + clear_formatting()
fun bright_bg_green(msg: String): String => "\033[102m" + msg + clear_formatting()
fun bright_bg_yellow(msg: String): String => "\033[103m" + msg + clear_formatting()
fun bright_bg_blue(msg: String): String => "\033[104m" + msg + clear_formatting()
fun bright_bg_magenta(msg: String): String => "\033[105m" + msg + clear_formatting()
fun bright_bg_cyan(msg: String): String => "\033[106m" + msg + clear_formatting()
fun bright_bg_white(msg: String): String => "\033[107m" + msg + clear_formatting()


