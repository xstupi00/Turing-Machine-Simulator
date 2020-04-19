/**
Module      : flp20-log
Description : Simulation of non-deterministic Turing Machine
Copyright   : (c) Simon Stupinsky, 2020
License     : GPL-3
Maintainer  : xstupi00@stud.fit.vutbr.cz
Project     : Logic project - Turing Machine
Course      : Functional and Logic Programming (FLP)
University  : University of Technology Brno (BUT)
Faculty     : Faculty of Information Technology (FIT)

This module implemented the simulation of the non-deterministic
Turing Machine. Firstly, it reads the transitions and the initial
tape from the given input. Then try to find the sequence of transition
to the final state. If such a sequence exists, it finds, otherwise, it
can abnormally be stopped or cycling to forever.
**/


/** Define the dynamic predicate which will be changing during execution. */
% transition([State, Symbol, NewState, NewSymbol])
% tapes([ConfigN, ConfingN_1, ..., Config0])
:- dynamic transition/1, paths/1.


/**
    Reads the lines from the standard output and
    terminates when it comes accross the EOF or LF.
*/
read_line(L, C) :-
    get_char(C),
        (isEOFEOL(C), L = [], ! ;
            read_line(LL, _),
            [C|LL] = L).


/** Test the given char on EOF or LF. */
isEOFEOL(C) :-
	C == end_of_file ;
	(char_code(C, Code), Code==10).


/** Reads lines and store it to the final list. */
read_lines(Ls) :-
    read_line(L,C),
    (C == end_of_file, Ls = [] ; read_lines(LLs), Ls = [L|LLs]).


% Matching the transition in the form:
%% [StartState, ' ', TapeSymbol, ' ', TargeState, ' ', NewSymbolLR]
%%% e.g. [S, ,a, ,B, ,a], [B, ,b, ,B, ,R],
split_transition(
    [StartState | [_ | [TapeSymbol |[_ | [TargetState | [_ | [NewSymbolLR]]]]]]],
    [StartState, TapeSymbol, TargetState, NewSymbolLR]
) :-
    % the states of the transition have to in the range [A-Z]
    char_type(StartState, upper), char_type(TargetState, upper),
    % the current symbol under the machine head have to in the range [a-z] or the space (epsilon)
    (char_type(TapeSymbol, lower) ; char_type(TapeSymbol, space)), (
        char_type(NewSymbolLR, lower) ; % new symbol to rewrite the current under the machine head
        char_type(NewSymbolLR, space) ; % current symbol without change and the need to consider it
        (char_code(NewSymbolLR, Code), Code == 76) ; % shift the machine head to the left (L)
        (char_code(NewSymbolLR, Code), Code == 82)   % shift the machine head to the right (R)
    ).
% Transitions with the invalid format - such transition are not stored
split_transition(_, []).


/** Write out the list of the symbols to the standard output. */
% e.g [S,a,a,a,c,a,a,a] => Saaacaaa
write_path([]).
write_path([Symbol|Symbols]) :- write(Symbol), write_path(Symbols).


/** Write out the list of the paths to the standard output. */
% e.g. [[F,a,a,a,c,a,a,a], [S,a,a,a,c,a,a,a]] => Faaacaa\nSaaacaa
write_paths([]).
write_paths([Path|Paths]) :- write_path(Path), nl, write_paths(Paths).


/** Assert a transitions into the database. */
store_transitions([]).
store_transitions([Transition|Transitions]) :-
    assert(transition(Transition)), store_transitions(Transitions).


/** Retract a given set of the path from the database. */
retract_previous_paths([]).
retract_previous_paths([Path|Paths]) :- retract(paths(Path)), retract_previous_paths(Paths).


/** Matching the initial content of the machine tape and convert it from string to atom. */
split_lines([InputTape],[], InputTape) :-
    % Convert the initial machine tape (string) to the atom for its verification.
    atom_string(InputTape, TapeStr),
    % All symbols in the initial tape have be in the range [a-z] or the spaces.
    forall(sub_atom(TapeStr, _, 1, _, C), (char_type(C, lower) ; char_type(C, space))).
/** Processing loaded lines to the form representing the transitions. */
split_lines([Line|Lines],[Transition|Transitions], InputTape) :-
    split_lines(Lines, Transitions, InputTape), split_transition(Line, Transition).

/** Matching the shift of the machine head to the right. */
perform_step(LeftTape, NewLeftTape, RightTape, NewRightTape, 'R') :-
    right_step(LeftTape, NewLeftTape, RightTape, NewRightTape), !.
/** Matching the shift of the machine head to the left. */
perform_step(LeftTape, NewLeftTape, RightTape, NewRightTape, 'L') :-
    left_step(LeftTape, NewLeftTape, RightTape, NewRightTape), !.
/** Matching the rewrite of the current symbol under the machine head. */
perform_step(LeftTape, LeftTape, [_|RightTape], [NewSymbol|RightTape], NewSymbol).


/** Perform the given set of the steps according to the given current configuration. */
perform_steps(_, _, [], []).
perform_steps(
    % current content of machine tape; new state and symbol from transition and store the result
    LeftTape, RightTape, [[State, Symbol]|Steps], [[NewLeftTape, State, NewRightTape]|NewTapes]
) :-
    perform_steps(LeftTape, RightTape, Steps, NewTapes), % recursive call for remaining steps
    perform_step(LeftTape, NewLeftTape, RightTape, NewRightTape, Symbol). % processing of one step


/**
  Shift the machine head to the right beyond the end of the tape, therefore it is added the space.
*/
right_step(LeftTape, NewLeftTape, [CurrentSymbol], [' ']) :-
    % Append the previous symbol under the machine head to the left part of the tape.
    append(LeftTape, [CurrentSymbol], NewLeftTape), !.
/** Shift the machine head to the right beyond the current symbol under the machine head. */
right_step(LeftTape, NewLeftTape, [CurrentSymbol|RightTape], RightTape) :-
    % Append the previous symbol under the machine head to the left part of the tape.
    append(LeftTape, [CurrentSymbol], NewLeftTape).


/**
  Shift the machine head to the left before the begin to the tape -  prohibited operation.
  Such a step will be ignored, and will not be further considered.
*/
left_step([], _, _, _) :- !, fail.
/** Shift the machine head to the left before the current symbol under the machine head. */
left_step(LeftTape, NewLeftTape, RightTape, [NewCurrentSymbol|RightTape]) :-
    % Obtained the last symbol from the last part of the tape.
    % Subsequently the addition of the obtained symbol to the right part of the tape.
    last(LeftTape, NewCurrentSymbol),
    % And also the deleting of this symbol from the left part of the tape.
    delete(LeftTape, NewCurrentSymbol, NewLeftTape).


/** Performs the initialization of the computation environment and run the simulation. */
step_counting(Transitions, InputTape) :-
    % Store the loaded transitions to the database.
    store_transitions(Transitions),
    % Append the initial configuration to the database.
    append(['S'], InputTape, InitPath), assert(paths([InitPath])),
    % Run the simulation from the initial configuration.
    steps([[[], 'S', InputTape]], 1).


/** Any next configuration was not found and therefore the machine has to stopped abnormally. */
steps([], _) :-
    % Construct the ErrorString with the relevant error message about the occurred state.
    atom_string(ErrStr, "ABNORMAL STOP: TS does not find the transitions to the final state F.\n"),
    write(ErrStr), halt. % Write out the error string and halt the whole program.
/** Any generated configuration is in the final state, then we halt the computation. */
steps(Configs, _) :- member([_, 'F', _], Configs).
/**  */
steps(Configs, Step) :-
    /** Find the all possible steps from the current configurations. */
    step(Configs, NewConfigs),
    /** Find all previous paths, that are shorter than the step which is currently performed. */
    findall(
        PreviousPaths,
        (paths(PreviousPaths), length(PreviousPaths, Len), Len =:= Step),
        PreviousPaths
    ),
    /** Retract the all founded paths, that were generated in the previous iterations. */
    retract_previous_paths(PreviousPaths),
    /** Increment the value of the step. */
    NextStep is Step + 1,
    /** Recursive call for the newly generated configurations in the current iteration. */
    steps(NewConfigs, NextStep).


/** Find the all possible transitions from the given set of the configurations. */
step([], []).
/** Match the current configuration, e.g., aaSacaaa */
step([[LeftTape, State, [Symbol|RightTape]]|Configs], NewConfigs) :-
    % Recursive call for the remaining configurations.
    step(Configs, TailConfigs),
    % Find all possible transition from the current configuration
    findall([NewState, NewSymbol], transition([State, Symbol, NewState, NewSymbol]), Steps),
    % Construct the new configurations according to the founded transitions and current config.
    perform_steps(LeftTape, [Symbol|RightTape], Steps, HeadConfig),
    % Actualize the reached path from the initial configuration about the constructed configs.
    store_paths([LeftTape, State, [Symbol], RightTape], HeadConfig),
    % Append the new configurations for the next processing.
    append(HeadConfig, TailConfigs, NewConfigs).


/** Construct and store the new path according to the given list of the new configurations. */
store_paths(_, []).
/** Match the previous configuration and the newly generated configuration. */
store_paths(PreviousConfigList, [ConfigList|Configs]) :-
    % Flatten the given lists from the most top levels.
    flatten(PreviousConfigList, PreviousConfig), flatten(ConfigList, Config),
    % Find the path which has the same previous configuration as who was generated the newly one.
    findall([PreviousConfig|MatchPath], paths([PreviousConfig|MatchPath]), StoredPathList),
    % Obtain the first founded path with the matched previous configuration.
    StoredPathList = [StoredPath|_],
    % Construct the new path with the new configuration.
    append([Config], StoredPath, NewPath),
    % Assert the newly constructed path to the database.
    assert(paths(NewPath)),
    % Recursive call for the remaining configurations.
    store_paths(PreviousConfigList, Configs).


/** Write out the found path which leads to the final state F from the database. */
write_final_path :-
    % Find all paths which were getting into the final state F in the last configuration.
    findall(
        [FinalPath|Paths], % match the last one configuration
        (paths([FinalPath|Paths]), member('F', FinalPath)), % check whether includes the final state
        FinalPaths % obtain the matched paths
    ),
    % Obtain the first founded path which included the final state.
    [ReversePaths|_] = FinalPaths,
    % Reverse the final path (the first item is the last configuration and vice versa)
    reverse(ReversePaths, Paths),
    % Write out the reversed final path.
    write_paths(Paths).


/** The main function of the whole module. */
main :-
    prompt(_, ''), % Set prompt associated with reading from the user_input stream.
    read_lines(LinesList), % Load lines from the standard input.
    split_lines(LinesList, Transitions_, InputTape), % Process loaded lines to working form.
    delete(Transitions_, [], Transitions), % Remove initial tape from the transitions list.
    step_counting(Transitions, InputTape), % Run the simulation of the Turing machine.
    write_final_path, % Write out the founded path to the final state.
    halt. % Halt the program.