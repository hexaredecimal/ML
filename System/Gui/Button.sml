
type JButton = "JButton"

fun button_new(text: String): JButton => {
  val btn = null of JButton
  java {
    "btn = new JButton(text);"
  }
  btn
}

fun button_set_onclick(btn: JButton, callback: fn(): Unit): Unit => {
  val msg = "clicked!!!!" 
  java {
    "btn.addActionListener(new ActionListener() {"
    "   @Override"
    "   public void actionPerformed(ActionEvent e) {"
    "     JOptionPane.showMessageDialog(null, msg);"
    "   }"
    "});"
  }
  ()
} 


