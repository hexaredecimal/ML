fun int_eq(): Bool => 42 == 42
fun bool_eq(): Bool => true == true
fun float_eq(): Bool => 42.0 == 42.0

fun int_neq(): Bool => 42 != 43
fun bool_neq(): Bool => true != false
fun float_neq(): Bool => 42.0 != 43.0

fun int_mul(): Bool => 2 * 2 == 4
fun float_mul(): Bool => 2.0 * 2.0 == 4.0

fun int_div(): Bool => 4 / 2 == 2
fun float_div(): Bool => 4.0 / 2.0 == 2.0

fun int_add(): Bool => 2 + 2 == 4
fun float_add(): Bool => 2.0 + 2.0 == 4.0

fun int_sub(): Bool => 4 - 2 == 2
fun float_sub(): Bool => 4.0 - 2.0 == 2.0

fun int_lt(): Bool => 2 < 4
fun float_lt(): Bool => 2.0 < 4.0

fun int_le(): Bool => 4 <= 4
fun float_le(): Bool => 4.0 <= 4.0

fun bool_and(): Bool => true && true
fun bool_or(): Bool => true || false

fun int_gt(): Bool => 4 > 2
fun float_gt(): Bool => 4.0 > 2.0

fun int_ge(): Bool => 4 >= 4
fun float_ge(): Bool => 4.0 >= 4.0


fun main(): Bool =>
    int_eq() &&
    bool_eq() &&
    float_eq() &&
    int_neq() &&
    bool_neq() &&
    float_neq() &&
    bool_and() &&
    bool_or() &&
    int_mul() &&
    float_mul() &&
    int_div() &&
    float_div() &&
    int_add() &&
    float_add() &&
    int_sub() &&
    float_sub() &&
    int_lt() &&
    float_lt() &&
    int_le() &&
    float_le() &&
    int_gt() &&
    float_gt() &&
    int_ge() &&
    float_ge()
