class SysConv {
  // Convert to Character
    
  public static Character to_Character(Object x) {
    return Character.valueOf(x.toString().charAt(0)); 
  }

  public static Character to_Character(Double x) {
    double value = x.doubleValue();
    int c = (int) value; 
    return Character.valueOf((char) c);
  }

  public static Character to_Character(Float x) {
    float value = x.floatValue(); 
    return Character.valueOf((char)value);
  }

  public static Character to_Character(Integer x) {
    int value = x.intValue(); 
    return Character.valueOf((char)value);
  }

  public static Character to_Character(Character x) {
    return x; 
  }

  public static Character to_Character(String x) {
    return x.charAt(0); 
  }

  // Convert to Float
  public static Float to_Float(Object x) {
    return Float.valueOf(x.toString()); 
  }

  public static Float to_Float(Double x) {
    double value = x.doubleValue(); 
    return Float.valueOf((float) value);
  }

  public static Float to_Float(Character x) {
    char value = x.charValue(); 
    return Float.valueOf((float) value);
  }

  public static Float to_Float(Integer x) {
    int value = x.intValue(); 
    return Float.valueOf(value);
  }

  public static Float to_Float(Float x) {
    return x; 
  }

  public static Float to_Float(String x) {
    return Float.parseFloat(x); 
  }

  // Convert to Integer
    
  public static Integer to_Integer(Object x) {
    return Integer.valueOf(x.toString()); 
  }

  public static Integer to_Integer(Double x) {
    double value = x.doubleValue(); 
    return Integer.valueOf((int) value);
  }

  public static Integer to_Integer(Character x) {
    char value = x.charValue(); 
    return Integer.valueOf(value);
  }

  public static Integer to_Integer(Float x) {
    float value = x.floatValue(); 
    return Integer.valueOf((int) value);
  }

  public static Integer to_Integer(Integer x) {
    return x; 
  }

  public static Integer to_Integer(String x) {
    return Integer.parseInt(x); 
  }


  // Convert to Double
  public static Double to_Double(Object x) {
    return Double.valueOf(x.toString()); 
  }

  public static Double to_Double(Integer x) {
    int value = x.intValue(); 
    return Double.valueOf((double) value * 1.0);
  }

  public static Double to_Double(Character x) {
    char value = x.charValue(); 
    return Double.valueOf((double) value);
  }

  public static Double to_Double(Float x) {
    float value = x.floatValue(); 
    return Double.valueOf((double) value);
  }

  public static Double to_Double(Double x) {
    return x; 
  }

  public static Double to_Double(String x) {
    return Double.parseDouble(x); 
  }
}

final class Intrinsic {
  public static Object panic(String msg) {
    try {
      throw new Exception(); 
    } catch (Exception e) {
      StackTraceElement[] ae = e.getStackTrace();
      int len = ae.length - 1; 

      StringBuilder sb = new StringBuilder();
      System.out.println(msg);
      for (int i = len; i >= 0; i--) {
        sb = sb.repeat("\t", len-i);
        if (i != len ) 
           sb = sb.append("└───⟴ ");
        System.out.println(sb.toString() + ae[i] + ((i == len - 2) ? " ⟸═══ Error Happened in this function / block": (i == len - 3) ? " ⟸═══ Error happened here 🐛" : ""));
        sb = new StringBuilder(); 
      }
      System.exit(0);
    }
    return ""; 
  }
}



