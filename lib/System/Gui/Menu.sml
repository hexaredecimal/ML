using System::Gui::MenuItem

type JMenu = "JMenu"

fun menu_new(title: String): JMenu => {
  val menu = null of JMenu
  java {
    "menu = new JMenu(title);"
  }
  menu
}

fun menu_add_menuitem(menu: JMenu, item: JMenuItem): Unit => {
  java {
    "menu.add(item);"
  }
  ()
}


fun menu_add_menu(menu: JMenu, other: JMenu): Unit => {
  java {
    "menu.add(other);"
  }
  ()
}

