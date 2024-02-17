
(*
*    =========================== SMLL Standard Libary =====================================
*                   File: System/File.smll
*                   Created by: Gama Sibusiso
*                   Date: 17-02-2024 
* *)

using System::Result

fun read_to_string(filename: String): Result => {
  val e = Result.Err("Failed to open file: " + filename) as Result
  java {
    "File fp = new File(filename);"
    "if (fp.exists() == false || fp == null)"
    " return e;"
    "else {"
    "  try {"
    "     Scanner sc = new Scanner(fp);"
    "     String st = new String();"
    "     while (sc.hasNextLine()) {"
    "       st += sc.nextLine() + '\n';"
    "     }"
    "     e = new Result.Ok(st);"
    "     sc.close();"
    "  } catch (Exception a) {"
    "     return e;"
    "  }"
    "}"
  }
  e
}

