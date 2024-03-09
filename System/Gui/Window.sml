using System::Gui::MenuBar
type JFrame = "JFrame"

fun window_new(title: String): JFrame => {
  val frame = null of JFrame
  java {
    "frame = new JFrame(title);"
  }
  frame
}

fun window_set_size(window: JFrame, width: Int, height: Int): Unit => {
  java {
    "window.setSize(width, height);"
  }
  ()
}

fun window_set_visible(window: JFrame, toggle: Bool): Unit => {
  java {
    "window.setVisible(toggle);"
  }
  ()
}

fun window_add(window: JFrame, child: Any): Unit => {
  java {
    "window.add((Component) child);"
  }
  ()
}

fun window_set_menubar(window: JFrame, menubar: JMenuBar): Unit => {
  java {
    "window.setJMenuBar(menubar);"
  }
  ()
}

