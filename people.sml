
fun println(...): Unit => {
  java {
    "for (Object arg: var_args) {"
    "  System.out.print(arg);"
    "}"
    "System.out.println();"
  }
  ()
} 

enum Peoples = 
  Male(pic: String, name: String)
  | Female(pic: String, name: String, age: Int)

fun male(name: String): Peoples.Male => Peoples.Male("👦", name)
fun female(name: String, age: Int): Peoples.Female => Peoples.Female("👧", name, age)

fun printMsg(person: Peoples): Unit => match person {
  Peoples.Male(pic, name) -> println("Hello ", pic, ". My name is ", name, " and I am a male") 
  Peoples.Female(pic, name, age) -> println("Hello ", pic, ". My name is ", name, " and I am ", age, " years old. I'm also a female") 
  _ -> ()
}

fun main(): Int => {
  val genders = [male("Adonis"), female("Mary", 25), female("Penny", 19), male("Jake")]
  printMsg(genders.[5])
  0
}
