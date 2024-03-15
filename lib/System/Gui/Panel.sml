
type JPanel = "JPanel"

fun panel_new(): JPanel => {
  val panel = null of JPanel
  java {
    "panel = new JPanel();"
  }
  panel
}

fun panel_add(panel: JPanel, child: Any): Unit => {
  java {
    "panel.add((Component) child);"
  }
  ()
}
