-module(stack_test).
-export([
  test_stack_push1/0
  ,test_stack_push2/0
  ,test_stack_pop1/0
  ,test_stack_pop2/0
  ,test_stack_top1/0
  ,test_stack_top2/0
]).

test_stack_push1() ->
  Stack = stack:new(),
	Stack1 = stack:push(Stack,1),
	erltest:assert_true(stack:size(Stack1) == 1).

test_stack_push2() ->
  Stack = stack:new(),
	Stack1 = stack:push(Stack,1),
	Stack2 = stack:push(Stack1,1),
	erltest:assert_true(stack:size(Stack2) == 2).


test_stack_pop1() ->
  Stack = stack:new(),
	Stack1 = stack:push(Stack,1),
	erltest:assert_true({1,[]} == stack:pop(Stack1)).

test_stack_pop2() ->
  Stack = stack:new(),
	Stack1 = stack:push(Stack,1),
	Stack2 = stack:push(Stack1,2),
	{Top1,Stack3} = stack:pop(Stack2),
	erltest:assert_true(2 == Top1 ),
	{Top2,_Stack4} = stack:pop(Stack3),
	erltest:assert_true(1 == Top2).

test_stack_top1() ->
  Stack = stack:new(),
	Stack1 = stack:push(Stack,1),
	Top = stack:top(Stack1),
	erltest:assert_true(1 == Top),
	erltest:assert_true(stack:size(Stack1) == 1).

test_stack_top2() ->
  Stack = stack:new(),
	Stack1 = stack:push(Stack,1),
	Stack2 = stack:push(Stack1,2),
	Top = stack:top(Stack2),
	erltest:assert_true(2 == Top ),
	erltest:assert_true(stack:size(Stack2) ==2 ).
