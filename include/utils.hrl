-ifndef(utils_hrl).
-define(utils_hrl, true).

%%
% ?WHEN(expr(), statement()) ->
%     statement() | undefined
%
% If Condition evaluates to true, execute Statement, otherwise return undefined.
%
-define(WHEN(Condition, Statement), case Condition of true -> Statement; _ -> undefined end).

% ?EITHER(expr(), statement(), statement()) ->
%     statement()
%
% If Condition evaluates to true, execute Statement1,
% otherwise execute Statement2
%
-define(EITHER(Condition, Statement1, Statement2), case Condition of true -> Statement1; _ -> Statement2 end).

-endif.
