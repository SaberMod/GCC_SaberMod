// { dg-options "-std=gnu++0x" }
// 2011-05-19  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2011-2013 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <type_traits>
#include <testsuite_hooks.h>
#include <testsuite_tr1.h>

void test01()
{
  bool test __attribute__((unused)) = true;
  using std::is_move_assignable;
  using namespace __gnu_test;

  // Positive tests.
  VERIFY( (test_property<is_move_assignable, int>(true)) );
  VERIFY( (test_property<is_move_assignable, float>(true)) );
  VERIFY( (test_property<is_move_assignable, EnumType>(true)) );
  VERIFY( (test_property<is_move_assignable, int*>(true)) );
  VERIFY( (test_property<is_move_assignable, int(*)(int)>(true)) );
  VERIFY( (test_property<is_move_assignable, int (ClassType::*)>(true)) );
  VERIFY( (test_property<is_move_assignable,
	   int (ClassType::*) (int)>(true)) );

  VERIFY( (test_property<is_move_assignable, NoexceptMoveAssignClass>(true)) );
  VERIFY( (test_property<is_move_assignable, ExceptMoveAssignClass>(true)) );
  VERIFY( (test_property<is_move_assignable, NoexceptCopyAssignClass>(true)) );
  VERIFY( (test_property<is_move_assignable, ExceptCopyAssignClass>(true)) );

  // Negative tests.
  VERIFY( (test_property<is_move_assignable, void>(false)) );
  VERIFY( (test_property<is_move_assignable, int[2]>(false)) );
  VERIFY( (test_property<is_move_assignable, float[][3]>(false)) );
  VERIFY( (test_property<is_move_assignable, EnumType[2][3][4]>(false)) );
  VERIFY( (test_property<is_move_assignable, int*[3]>(false)) );
  VERIFY( (test_property<is_move_assignable, int(*[][2])(int)>(false)) );
  VERIFY( (test_property<is_move_assignable,
	   int (ClassType::*[2][3])>(false)) );
  VERIFY( (test_property<is_move_assignable, 
	   int (ClassType::*[][2][3]) (int)>(false)) );

  VERIFY( (test_property<is_move_assignable, DeletedCopyAssignClass>(false)) );
  VERIFY( (test_property<is_move_assignable, DeletedMoveAssignClass>(false)) );
}

int main()
{
  test01();
  return 0;
}
