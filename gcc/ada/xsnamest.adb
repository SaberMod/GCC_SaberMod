------------------------------------------------------------------------------
--                                                                          --
--                          GNAT SYSTEM UTILITIES                           --
--                                                                          --
--                             X S N A M E S T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2008, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This utility is used to make a new version of the Snames package when new
--  names are added. This version reads a template file from snames.adt in
--  which the numbers are all written as $, and generates a new version of
--  the spec file snames.ads (written to snames.ns). It also reads snames.adb
--  and generates an updated body (written to snames.nb), and snames.h and
--  generates an updated C header file (written to snames.nh).

with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Strings.Maps;              use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants;    use Ada.Strings.Maps.Constants;
with Ada.Text_IO;                   use Ada.Text_IO;

with GNAT.Spitbol;                  use GNAT.Spitbol;
with GNAT.Spitbol.Patterns;         use GNAT.Spitbol.Patterns;

procedure XSnamesT is

   InB  : File_Type;
   InT  : File_Type;
   OutS : File_Type;
   OutB : File_Type;
   InH  : File_Type;
   OutH : File_Type;

   A, B  : VString := Nul;
   Line  : VString := Nul;
   Name0 : VString := Nul;
   Name1 : VString := Nul;
   Oval  : VString := Nul;
   Restl : VString := Nul;

   Name_Ref : constant Pattern := Span (' ') * A & Break (' ') * Name0
                                  & Span (' ') * B
                                  & ": constant Name_Id := N + $;"
                                  & Rest * Restl;

   Get_Name : constant Pattern := "Name_" & Rest * Name1;
   Chk_Low  : constant Pattern := Pos (0) & Any (Lower_Set) & Rest & Pos (1);
   Findu    : constant Pattern := Span ('u') * A;

   Val : Natural;

   Xlate_U_Und : constant Character_Mapping := To_Mapping ("u", "_");

   M : Match_Result;

   type Header_Symbol is (None, Name, Attr, Conv, Prag);
   --  A symbol in the header file

   procedure Output_Header_Line (S : Header_Symbol);
   --  Output header line

   Header_Name : aliased String := "Name";
   Header_Attr : aliased String := "Attr";
   Header_Conv : aliased String := "Convention";
   Header_Prag : aliased String := "Pragma";
   --  Prefixes used in the header file

   type String_Ptr is access all String;
   Header_Prefix : constant array (Header_Symbol) of String_Ptr :=
                     (null,
                      Header_Name'Access,
                      Header_Attr'Access,
                      Header_Conv'Access,
                      Header_Prag'Access);

   --  Patterns used in the spec file

   Get_Attr : constant Pattern := Span (' ') & "Attribute_"
                                  & Break (",)") * Name1;
   Get_Conv : constant Pattern := Span (' ') & "Convention_"
                                  & Break (",)") * Name1;
   Get_Prag : constant Pattern := Span (' ') & "Pragma_"
                                  & Break (",)") * Name1;

   type Header_Symbol_Counter is array (Header_Symbol) of Natural;
   Header_Counter : Header_Symbol_Counter := (0, 0, 0, 0, 0);

   Header_Current_Symbol : Header_Symbol := None;
   Header_Pending_Line : VString := Nul;

   ------------------------
   -- Output_Header_Line --
   ------------------------

   procedure Output_Header_Line (S : Header_Symbol) is
      function Make_Value (V : Integer) return String;
      --  Build the definition for the current macro (Names are integers
      --  offset to N, while other items are enumeration values).

      function Make_Value (V : Integer) return String is
      begin
         if S = Name then
            return "(First_Name_Id + 256 + " & V & ")";
         else
            return "" & V;
         end if;
      end Make_Value;

   begin
      --  Skip all the #define for S-prefixed symbols in the header.
      --  Of course we are making implicit assumptions:
      --   (1) No newline between symbols with the same prefix.
      --   (2) Prefix order is the same as in snames.ads.

      if Header_Current_Symbol /= S then
         declare
            Name2 : Vstring;
            Pat : constant Pattern := "#define  "
                                       & Header_Prefix (S).all
                                       & Break (' ') * Name2;
            In_Pat : Boolean := False;

         begin
            if Header_Current_Symbol /= None then
               Put_Line (OutH, Header_Pending_Line);
            end if;

            loop
               Line := Get_Line (InH);

               if Match (Line, Pat) then
                  In_Pat := True;
               elsif In_Pat then
                  Header_Pending_Line := Line;
                  exit;
               else
                  Put_Line (OutH, Line);
               end if;
            end loop;

            Header_Current_Symbol := S;
         end;
      end if;

      --  Now output the line

      --  Note that we must ensure at least one space between macro name and
      --  parens, otherwise the parenthesized value gets treated as an argument
      --  specification.

      Put_Line (OutH, "#define  " & Header_Prefix (S).all
                  & "_" & Name1
                  & (30 - Natural'Min (29, Length (Name1))) * ' '
                  & Make_Value (Header_Counter (S)));
      Header_Counter (S) := Header_Counter (S) + 1;
   end Output_Header_Line;

--  Start of processing for XSnames

begin
   Open (InT, In_File, "snames.ads-tmpl");
   Open (InB, In_File, "snames.adb-tmpl");
   Open (InH, In_File, "snames.h-tmpl");

   Create (OutS, Out_File, "snames.ns");
   Create (OutB, Out_File, "snames.nb");
   Create (OutH, Out_File, "snames.nh");

   Anchored_Mode := True;
   Val := 0;

   loop
      Line := Get_Line (InB);
      exit when Match (Line, "   Preset_Names");
      Put_Line (OutB, Line);
   end loop;

   Put_Line (OutB, Line);

   LoopN : while not End_Of_File (InT) loop
      Line := Get_Line (InT);

      if not Match (Line, Name_Ref) then
         Put_Line (OutS, Line);

         if Match (Line, Get_Attr) then
            Output_Header_Line (Attr);
         elsif Match (Line, Get_Conv) then
            Output_Header_Line (Conv);
         elsif Match (Line, Get_Prag) then
            Output_Header_Line (Prag);
         end if;
      else
         Oval := Lpad (V (Val), 3, '0');

         if Match (Name0, "Last_") then
            Oval := Lpad (V (Val - 1), 3, '0');
         end if;

         Put_Line
           (OutS, A & Name0 & B & ": constant Name_Id := N + "
            & Oval & ';' & Restl);

         if Match (Name0, Get_Name) then
            Name0 := Name1;
            Val   := Val + 1;

            if Match (Name0, Findu, M) then
               Replace (M, Translate (A, Xlate_U_Und));
               Translate (Name0, Lower_Case_Map);

            elsif not Match (Name0, "Op_", "") then
               Translate (Name0, Lower_Case_Map);

            else
               Name0 := 'O' & Translate (Name0, Lower_Case_Map);
            end if;

            if Name0 = "error" then
               Name0 := V ("<error>");
            end if;

            if not Match (Name0, Chk_Low) then
               Put_Line (OutB, "     """ & Name0 & "#"" &");
            end if;

            Output_Header_Line (Name);
         end if;
      end if;
   end loop LoopN;

   loop
      Line := Get_Line (InB);
      exit when Match (Line, "     ""#"";");
   end loop;

   Put_Line (OutB, Line);

   while not End_Of_File (InB) loop
      Line := Get_Line (InB);
      Put_Line (OutB, Line);
   end loop;

   Put_Line (OutH, Header_Pending_Line);
   while not End_Of_File (InH) loop
      Line := Get_Line (InH);
      Put_Line (OutH, Line);
   end loop;
end XSnamesT;
