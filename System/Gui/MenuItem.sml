
type JMenuItem = "JMenuItem"

fun menuitem_new(title: String): JMenuItem => {
  val menuitem = null of JMenuItem
  java {
    "menuitem = new JMenuItem(title);"
  }
  menuitem
}

fun menuitem_set_onclick(item: JMenuItem, callback: fn(): Unit): Unit => {
  java {
    "item.addActionListener(new ActionListener() {"
    "   @Override"
    "   public void actionPerformed(ActionEvent e) {"
    "     callback.apply();"
    "   }"
    "});"
  }
  ()
} 

