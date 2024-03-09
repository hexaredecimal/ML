
(*
*    =========================== SMLL Standard Libary =====================================
*                   File: System/Stdio.smll
*                   Created by: Gama Sibusiso
*                   Date: 17-02-2024 
* *)

type StdFileId = Int
fun stdin(): StdFileId => 0
fun stdout(): StdFileId => 1
fun stderr(): StdFileId => 2


