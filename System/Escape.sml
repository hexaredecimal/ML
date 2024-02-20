
(*
*    =========================== SMLL Standard Libary =====================================
*                   File: System/Io.smll
*                   Created by: Gama Sibusiso
*                   Date: 20-02-2024 
* *)

(* Text effects *)
fun clear(): String => "\033[0m"
fun bold(c: String): String => "\033[1m" + c + clear()
fun underline(c: String): String => "\033[4m" + c + clear()
fun block(c: String): String => "\033[7m" + c + clear()
fun strikethrough(c: String): String => "\033[9m" + c + clear()
fun double_underline(c: String): String => "\033[21m" + c + clear()

(* Foreground colors *)
fun fg_black(msg: String): String => "\033[30m" + msg + "\033[0m"
fun fg_red(msg: String): String => "\033[31m" + msg + "\033[0m"
fun fg_green(msg: String): String => "\033[32m" + msg + "\033[0m"
fun fg_yellow(msg: String): String => "\033[33m" + msg + "\033[0m"
fun fg_blue(msg: String): String => "\033[34m" + msg + "\033[0m"
fun fg_magenta(msg: String): String => "\033[35m" + msg + "\033[0m"
fun fg_cyan(msg: String): String => "\033[36m" + msg + "\033[0m"
fun fg_white(msg: String): String => "\033[37m" + msg + "\033[0m"

(* Background ground colors *)
fun bg_black(msg: String): String => "\033[40m" + msg + "\033[0m"
fun bg_red(msg: String): String => "\033[41m" + msg + "\033[0m"
fun bg_green(msg: String): String => "\033[42m" + msg + "\033[0m"
fun bg_yellow(msg: String): String => "\033[43m" + msg + "\033[0m"
fun bg_blue(msg: String): String => "\033[44m" + msg + "\033[0m"
fun bg_magenta(msg: String): String => "\033[45m" + msg + "\033[0m"
fun bg_cyan(msg: String): String => "\033[46m" + msg + "\033[0m"
fun bg_white(msg: String): String => "\033[47m" + msg + "\033[0m"


(* Bright Foreground colors *)
fun bright_fb_black(msg: String): String => "\033[90m" + msg + "\033[0m"
fun bright_fb_red(msg: String): String => "\033[91m" + msg + "\033[0m"
fun bright_fb_green(msg: String): String => "\033[92m" + msg + "\033[0m"
fun bright_fb_yellow(msg: String): String => "\033[93m" + msg + "\033[0m"
fun bright_fb_blue(msg: String): String => "\033[94m" + msg + "\033[0m"
fun bright_fb_magenta(msg: String): String => "\033[95m" + msg + "\033[0m"
fun bright_fb_cyan(msg: String): String => "\033[96m" + msg + "\033[0m"
fun bright_fb_white(msg: String): String => "\033[97m" + msg + "\033[0m"

(* Bright Background ground colors *)
fun bright_bg_black(msg: String): String => "\033[100m" + msg + "\033[0m"
fun bright_bg_red(msg: String): String => "\033[101m" + msg + "\033[0m"
fun bright_bg_green(msg: String): String => "\033[102m" + msg + "\033[0m"
fun bright_bg_yellow(msg: String): String => "\033[103m" + msg + "\033[0m"
fun bright_bg_blue(msg: String): String => "\033[104m" + msg + "\033[0m"
fun bright_bg_magenta(msg: String): String => "\033[105m" + msg + "\033[0m"
fun bright_bg_cyan(msg: String): String => "\033[106m" + msg + "\033[0m"
fun bright_bg_white(msg: String): String => "\033[107m" + msg + "\033[0m"


