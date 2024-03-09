using System::Gui::Menu

type JMenuItem = "JMenuItem"

fun menuitem_new(title: String): JMenuItem => {
  val menuitem = null of JMenuItem
  java {
    "menuitem = new JMenuItem(title);"
  }
  menuitem
}


