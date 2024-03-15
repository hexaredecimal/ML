using System::Gui::Menu

type JMenuBar = "JMenuBar"

fun menubar_new(): JMenuBar => {
  val menubar = null of JMenuBar
  java {
    "menubar = new JMenuBar();"
  }
  menubar
}

fun menubar_add_menu(bar: JMenuBar, menu: JMenu): Unit => {
  java {
    "bar.add(menu);"
  }
  ()
}
