%
%    Jabber.se Web Application
%    Copyright (C) 2011 Jonas Ådahl
%
%    This program is free software: you can redistribute it and/or modify
%    it under the terms of the GNU Affero General Public License as
%    published by the Free Software Foundation, either version 3 of the
%    License, or (at your option) any later version.
%
%    This program is distributed in the hope that it will be useful,
%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%    GNU Affero General Public License for more details.
%
%    You should have received a copy of the GNU Affero General Public License
%    along with this program.  If not, see <http://www.gnu.org/licenses/>.
%

-module(validator_maybe_email).
-export(
    [
        render_action/1, validate/2
    ]).

-include_lib("nitrogen_core/include/wf.hrl").

-include("include/ui.hrl").

render_action(Record)  ->
    TriggerPath= Record#maybe_email.trigger,
    TargetPath = Record#maybe_email.target,
    Text = wf:js_escape(Record#maybe_email.text),
    validator_custom:render_action(#custom { 
        trigger=TriggerPath, 
        target=TargetPath, 
        function=fun validate/2, text = Text, tag=Record 
    }),
    wf:f("v.add(Validate.Custom, {\
        against: function(values, args) {\
            if (values)\
                return Validate.Email(values);
            else\
                return true;\
        },\
        failureMessage: \"~s\"\
    });", [Text]).

validate(_, undefined) -> true;
validate(_, Value) ->
    case re:run(wf:to_list(Value), "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]+") of
        {match, _} -> true;
        _ -> false
    end.

