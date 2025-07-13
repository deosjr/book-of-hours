% game starts with one Health, and one of Chor/Shapt/Fet (I chose Shapt)
% next choice is Mettle/Phost/Ereb/Wist (I chose Wist) (does this set depend on the first choice?)
% next choice is what to read the journal with. I chose Shapt (gain another Shapt card).
% -> sand-scarred journal (knock, mystery(winter), memories:the archaeologist)
% -> 3x lesson: inks of containment, 1x lesson: wolf stories
% -> root of the tree: gain Trist

:- use_module(library(clpfd)).

:- discontiguous card/2.
:- discontiguous slots/3.

run :-
/*
	read_book(the_queens_of_the_rivers, S),
	writeln(S).
*/
	get_assistance(long_tower_first_floor, S),
	list_to_set(S, Set), % todo ensure no duplicates instead of postprocessing?
	foreach( member(X, Set), writeln(X) ).
	%include(member(memory(sunny)), Set, Steps),
	%writeln(Steps).

get_assistance(Lock, Steps) :-
	findall(List, assistance_steps(Lock, List), Steps).

/*
assistance_steps(Lock, Steps) :-
	lock(Lock, _),
	slots(Lock, [Slot], []),
	card(Card),
	Card = card(Name, _),
	satisfy(Slot, Card),
	Steps = [assistant(Name)].
*/

assistance_steps(Lock, Steps) :-
	lock(Lock, _),
	slots(Lock, [Slot], []),
	slots(talk, [To, About], [Pred]),
	card(Assistant),
	satisfy(To, Assistant),
	card(Enhancement),
	Assistant = card(Name, _),
	Enhancement = card(CardName, _),
	CardName \= lesson(_), % dont want to use lessons
	satisfy(About, Enhancement),
	call(Pred, Assistant, Enhancement, Enhanced),
	satisfy(Slot, Enhanced),
	Steps = [assistant(Name), card(CardName)].

card(C) :-
	card(Name, Attrs),
	C = card(Name, Attrs).

% default assumption: locks require assistance
slots(Name, Slots, []) :-
	lock(Name, Reqs),
	Slots = [slot(unlock, [assistance], Reqs, [])].

% this is a follow-up, modelled as two slots
slots(talk, [To, About], [enhance_soul]) :-
	To = slot(talk, [], [assistance], [collaboration]),
	About = slot(collaborate, [], [soul], []).
slots(talk, [To, About], [enhance_memory]) :-
	To = slot(talk, [], [assistance], [exalted(memory)]),
	About = slot(collaborate, [], [memory], []).
slots(talk, [To, About], [enhance_tool]) :-
	To = slot(talk, [], [assistance], [exalted(tool)]),
	About = slot(collaborate, [], [tool], []).
slots(talk, [To, About], [enhance_beverage]) :-
	To = slot(talk, [], [assistance], [exalted(beverage)]),
	About = slot(collaborate, [], [beverage], []).

enhance_soul(Assistent, Soul, Out) :-
	Assistent = card(Name, Attrs),
	Soul = card(_, SoulAttrs),
	enhance_principles(Attrs, SoulAttrs, NewAttrs),
	Out = card(Name, [collaboration|NewAttrs]).

enhance_memory(Assistent, Memory, Out) :-
	Assistent = card(Name, Attrs),
	Memory = card(_, MemAttrs),
	enhance_principles(Attrs, MemAttrs, NewAttrs),
	Out = card(Name, [exalted(memory)|NewAttrs]).

enhance_tool(Assistent, Tool, Out) :-
	Assistent = card(Name, Attrs),
	Tool = card(_, ToolAttrs),
	enhance_principles(Attrs, ToolAttrs, NewAttrs),
	Out = card(Name, [exalted(tool)|NewAttrs]).

enhance_beverage(Assistent, Beverage, Out) :-
	Assistent = card(Name, Attrs),
	Beverage = card(_, BevAttrs),
	enhance_principles(Attrs, BevAttrs, NewAttrs),
	Out = card(Name, [exalted(beverage)|NewAttrs]).

enhance_principles(Attrs, AddAttrs, NewAttrs) :-
	include(principle_attr, AddAttrs, PAttrs),
	add_attrs(Attrs, PAttrs, NewAttrs).

principle_attr(P) :- principle(P).
principle_attr(P-_) :- principle(P).
principle(heart).
principle(nectar).
principle(scale).
principle(forge).
principle(knock).
principle(sky).
principle(lantern).
principle(winter).
principle(grail).
principle(edge).
principle(rose).
principle(moth).
principle(moon).

add_attrs([], X, X).
add_attrs([H|T], List, Out) :-
	H \= -(_,_),
	( member(H, List) ->
		select(H, List, H-2, NewList),
		add_attrs(T, NewList, Out)
	;
		( member(H-X, List) ->
			N #= X+1,
			select(H-X, List, H-N, NewList),
			add_attrs(T, NewList, Out)
		;
			add_attrs(T, [H|List], Out)
		)
	).
add_attrs([H-X|T], List, Out) :-
	( member(H, List) ->
		N #= X+1,
		select(H, List, H-N, NewList),
		add_attrs(T, NewList, Out)
	;
		( member(H-Y, List) ->
			N #= X+Y,
			select(H-Y, List, H-N, NewList),
			add_attrs(T, NewList, Out)
		;
			add_attrs(T, [H-X|List], Out)
		)
	).

%% CARDS %%
% card(name, [attributes])

%% SOULS %%
soul(card(Name, Attrs)) :-
	card(Name, Attrs),
	member(soul, Attrs).

card(health, [soul, heart, nectar, scale, occupant]).
card(shapt, [soul, forge, knock-2, occupant]).
card(wist, [soul, lantern, winter-2, occupant]).
card(trist, [soul, moon, moth-2, occupant]).
card(mettle, [soul, edge, forge-2, occupant]).
card(ereb, [soul, edge, grail-2, occupant]).
card(phost, [soul, lantern-2, sky, occupant]).
card(fet, [soul, moth, rose-2, occupant]).
card(chor, [soul, grail, heart-2, occupant]).

% inks of containment at telescope: improve mettle
card(mettle_plus, [edge-2, forge-3, soul, occupant]).

%% MEMORIES %%
memory(card(Name, Attrs)) :-
	card(Name, Attrs),
	member(memory, Attrs).

card(fear, [memory, edge, scale-2]).
card(satisfaction, [memory, grail-2, heart]).
card(solace, [memory, heart-2, sky]).
card(gossip, [memory, grail, rose-2]).
card(revelation, [memory, lantern-2]).
card(occult_scrap, [memory, knock-2, moth-2, rose-2, evolve(horomachistry), persistent, omen]).
card(salt, [memory, knock, moon, winter]).
card(confounding_parable, [memory, moon-2, rose-2, sky-2]).
card(impulse, [memory, moth-2, nectar]).
card(foresight, [memory, forge-2, lantern]).
card(hindsight, [memory, scale, winter-2]).
card(contradiction, [memory, edge-2, moon]).

%% WEATHER %%
card(rain, [memory, weather, grail-2, nectar-2]).	% spring, summer
card(gale, [memory, weather, heart-3, sky-3]).		% spring
card(sunny, [memory, weather, lantern-2, sky-2]).	% spring, summer
card(clouds, [memory, weather, moon, moth]).		% spring
card(fog, [memory, weather, knock-3, moon-3]).		% autumn
card(storm, [memory, weather, heart-4, sky-4]).		% autumn

% format for slots: slot_type(name, inputlist, outputlist).
% input is requirements per slot, output is name of card

%% SLOTS %%
% slots(name, [Slot, ...], [Output, ...]) % challenges, forms?
% Slot = slot(name, Essential, Required, Forbidden)
% Output = cardname
% todo: optional slots?

satisfy(Slot, Card) :-
	Slot = slot(_, Essential, Required, Forbidden),
	req(Essential, Required, Forbidden, Card).

satisfy(Slot, Available, Cards) :-
	include(satisfy(Slot), Available, Cards).

req(Essential, Required, Forbidden, Card) :-
	meet_essential_reqs(Essential, Card),
	meet_required_reqs(Required, Card),
	( Forbidden = [] -> true ;
		not(meet_required_reqs(Forbidden, Card))).

meet_essential_reqs([], _).
meet_essential_reqs([E], card(_, Attributes)) :-
	member(E, Attributes).

meet_required_reqs([], _).
meet_required_reqs(Required, card(_, Attributes)) :-
	member(A, Required),
	member(A, Attributes).
meet_required_reqs(Required, card(_, Attributes)) :-
	member(A, Required),	% this is implicitly A-1, and any A-X will be 1 or higher
	member(A-_, Attributes).
meet_required_reqs(Required, card(_, Attributes)) :-
	member(A-X, Required),
	member(A-Y, Attributes), % any A without dash is 1, and will always fail to meet an A-X
	X #=< Y.

%% VILLAGE %%
% todo: mrs kille
slots(mrs_and_mr_kille, [slot(the_front_room, [soul], [grail], [fatigued, malady])], [midwifes_assistance]).
slots(mrs_and_mr_kille, [slot(the_front_room, [soul], [winter], [fatigued, malady])], [coffinmakers_assistance]).

% todo: follow-up requires a shilling
slots(rectory, [slot(the_study, [soul], [knock, lantern], [fatigued, malady])], [rectors_assistance]).
slots(blacksmith, [slot(a_smoky_room, [soul], [edge, forge, heart], [fatigued, malady])], [blacksmiths_assistance]).

slots(seas_edge, [slot(object, [], [soul, heart], [fatigued, malady])], [salt]).

%% ASSISTANCE %%

card(coffinmakers_assistance, [assistance, sky, winter-2, introduction]).
card(midwifes_assistance, [assistance, heart, grail-2, introduction]).
card(rectors_assistance, [assistance, knock, lantern-2, introduction]).
card(blacksmiths_assitance, [assistance, edge-2, forge-2, introduction]).

%% BEDS %%
slots(Name, Slots, Output) :-
	bed(Name, Reqs),
	Slots = [
		slot(soul, [soul], Reqs, []),
		slot(skill, [skill], Reqs, []),
		slot(memory, [memory], Reqs, []),
		slot(restore, [], [fatigued, malady], []),
		slot(with, [], [restorative, beverage], [])
	],
	Output = []. %todo

bed(lodge, [scale, forge, knock, heart]).
bed(solomon, [rose, lantern, winter, heart]).

%% LOCKS %%

% default assumption: locks require assistance
slots(Name, Slots, []) :-
	lock(Name, Reqs),
	Slots = [slot(unlock, [assistance], Reqs, [])].

% lock(great_gate, key) % todo: doesnt need assistance..
% exception: great gate needs a key
slots(great_gate, [slot(unlock, [key], [knock, thing], [])], []).

lock(cucurbit_bridge, [forge, lantern, sky]).
lock(keepers_lodge, [forge-2, heart, knock]).
lock(gatehouse_stairs, [forge-2, lantern-2]).
lock(cloistered_garden, [lantern-2, nectar-2]).
lock(watchmans_tower_top_floor, [grail-2, sky-2]).
lock(long_tower_first_floor, [lantern-3, winter-3]).
lock(walled_garden, [edge-4, nectar-4]).
lock(long_tower_second_floor, [lantern-4, winter-4]).
lock(first_true_threshold, [knock-3, rose-3]).
lock(tangle_of_thorns, [grail-4, nectar-4]).
lock(weed_choked_garden, [heart-3, nectar-3]).
lock(grand_ascent_ground_floor, [edge-4, heart-4, moon-4, winter-4]).
lock(ruinous_hall, [heart-5, scale-5]).
lock(grand_ascent_first_floor, [forge-4, heart-4, sky-4, winter-4]).
lock(infirmary, [heart-4, rose-4]).

%% BOOKS %%
book(card(Name, Attrs)) :-
	card(Name, Attrs),
	member(codex, Attrs).

card(Name, Attrs) :-
	book(Name, Mystery, N, Period, Subject, Tally, Opts),
	principle(Mystery),
	Attrs = [codex, mystery(Mystery)-N, readable, thing, period(Period), subject(Subject), tally_price(Tally)|Opts].

book(travelling_at_night_vol1, knock, 4, nocturnal, edicts_liminal, 1, []).
book(travelling_at_night_vol2, sky, 6, nocturnal, sights_and_sensations, 1, []).
book(the_wound_wounds, edge, 18, curia, disciplines_of_the_scar, 2, [written(killasimi)]).
book(exercises_in_the_continuity_of_self, winter, 8, curia, rhyme_and_remembrance, 2, []).
book(the_five_letters_on_memory, winter, 6, curia, rhyme_and_remembrance, 2, []).
book(the_queens_of_the_rivers, edge, 4, nocturnal, path_and_pilgrim, 1, []).
book(a_sevent_voice, sky, 8, baronial, preliminal_meter, 3, []).
book(the_republic_of_teeth, scale, 6, baronial, serpents_and_venoms, 3, []).
book(a_true_and_complete_accounting_of_the_asclepian_mysteries_of_the_roots_of_the_house, heart, 4, curia, weaving_and_knotworking, 2, []).
book(the_geminiad_ii, grail, 10, curia, pentiments_and_precursors, 2, [written(fucine)]).
book(an_almanac_of_entrances, knock, 10, curia, edicts_liminal, 2, []).
book(the_devouted_tantra, grail, 6, curia, applebright_euphonies, 2, [written(sanskrit)]).
book(de_horis_book_2, edge, 4, baronial, disciplines_of_the_hammer, 3, [written(latin)]).
book(the_bee_keepers_ends, nectar, 8, baronial, insects_and_nectars, 3, []).
book(those_indignities_perpetrated_by_the_deceitful_fraternity_of_obliviates, winter, 6, baronial, ragged_crossroads, 3, [written(latin)]).
book(deaths_and_their_evasions, knock, 14, baronial, edicts_liminal, 3, [written(fucine)]).
book(the_treatise_on_underplaces, knock, 4, baronial, horns_and_ivories, 3, []).
book(one_hundred_and_eight, scale, 4, baronial, pentiments_and_precursors, 3, []).
book(sunrise_awakenings, lantern, 4, baronial, auroral_contemplations, 3, []).
book(the_carbonek_schism, moth, 6, baronial, sacra_limiae, 3, [written(sabazine)]).

book_read(the_devoured_tantra, [confounding_parable, lesson(applebright_euphonies)]).
book_read(travelling_at_night_vol2, [solace, lesson(sights_and_sensations)]).
book_read(a_true_and_complete_accounting_of_the_asclepian_mysteries_of_the_roots_of_the_house, [impulse, lesson(weaving_and_knotworking)]).
book_read(a_seventh_voice, [confounding_parable, lesson(preliminal_meter)-2]).
book_read(sunrise_awakenings, [revelation, lesson(auroral_contemplations)]).
book_read(de_horis_book_2, [foresight, lesson(disciplines_of_the_hammer)]).
book(those_indignities_perpetrated_by_the_deceitful_fraternity_of_obliviates, [contradiction, lesson(ragged_crossroads)]).

%% DESKS %%

slots(Name, Slots, Output) :-
	desk(Name, Reqs),
	Slots = [
		slot(soul, [soul], Reqs, []),
		slot(skill, [skill], Reqs, []),
		slot(memory, [memory], Reqs, []),
		slot(papers, [], [readable, blank], [phonograph_record, reel_of_film]),
		slot(with, [], [tool, ink], [fatigued])
	],
	Output = []. %todo

desk(eva, [edge, grail, moon, winter]).
desk(pale, [lantern, scale, sky, winter]).

%% WORKSTATIONS %%

% challenge: horomachistry
slots(telescope, Slots, Output) :-
	Reqs = [forge, moon, rose, sky],
	Slots = [
		slot(soul, [soul], Reqs, []),
		slot(skill, [skill], Reqs, []),
		slot(memory, [memory], Reqs, []),
		slot(+, [], [soul, lens, tool], [fatigued]),
		slot(+, [], [lens, memory, tool], [fatigued])
	],
	Output = []. %todo

%% TOOLS %%

card(mirrorscope, [lantern, thing, tool]).
card(baron_silences_astrolabe, [sky, comfort, tool]).

%% BEVERAGES %%
beverage(card(Name, Attrs)) :-
	card(Name, Attrs),
	member(beverage, Attrs).

card(dandelion_wine, [lantern, nectar-2, thing, liquid, beverage, restorative, intoxicating, cooking_ingredient]).
card(c_and_h_second_flush_assam, [edge, knock, lantern, thing, liquid, beverage, restorative]).
card(dawnlion_coffee, [forge, lantern-2, scale, thing, liquid, beverage, restorative]).
card(mist_kissed_water, [knock-2, moon-2, winter, heart, thing, liquid, beverage, restorative]).
card(isle_water, [heart, moon, winter, thing, liquid, beverage, restorative]).
card(skinshuck_mead, [scale-2, heart-2, moth-6, nectar-2, thing, liquid, beverage, restorative, intoxicating, cooking_ingredient]).
card(eigengrau, [winter, moon, thing, liquid, beverage, restorative, intoxicating, cooking_ingredient]).
card(roscraggan_whisky, [rose-2, forge, thing, liquid, beverage, restorative, intoxicating, cooking_ingredient]).

%% TALLY %%
card(bronze_spintria, [forge-2, ductile, tally(2)]).

%% LESSONS %%

card(lesson(inks_of_containment), [moon, winter, memory, persistent, lesson]).
card(lesson(wolf_stories), [moon, scale, memory, persistent, lesson]).
card(lesson(applebright_euphonies), [grail, sky, memory, persistent, lesson]).
card(lesson(sights_and_sensations), [sky, winter, memory, persistent, lesson]).
card(lesson(weaving_and_knotworking), [heart, moth, memory, persistent, lesson]).
card(lesson(preliminal_meter), [knock, rose, memory, persistent, lesson]).
card(lesson(auroral_contemplations), [edge, lantern, memory, persistent, lesson]).
card(lesson(disciplines_of_the_hammer), [edge, forge, memory, persistent, lesson]).
card(lesson(ragged_crossroads), [edge, winter, memory, persistent, lesson]).

%% SKILLS %%

card(deep_mandaic, [forge-2, lantern, skill, language, horomachistry, ithastry]).

card(inks_of_containment, [moon, winter-2, skill, horomachistry, preservation, effective(theoplasmic_contamination)]).
card(wolf_stories, [moon-2, scale, skill, birdsong, skolekosophy]).
card(applebright_euphonies, [grail, sky-2, skill, the_bosk, illumination]).
card(sights_and_sensations, [sky-2, winter, skill, hushery, nyctodromy]).
card(weaving_and_knotworking, [heart-2, moth, skill, birdsong, the_bosk]).
card(preliminal_meter, [knock-2, rose, skill, illumination, ithastry]).
card(auroral_contemplations, [edge, lantern-2, skill, illumination, nyctodromy]).
card(disciplines_of_the_hammer, [edge-2, forge, skill, illumination, nyctodromy]).
card(ragged_crossroads, [edge-2, winter, skill, illumination, skolekosophy]).

%% SKILL TREE %%
tree(horomachistry, 1, inks_of_containment, mettle). % <- chosen
tree(preservation, 1, inks_of_containment, heart).
tree(skolekosophy, 1, wolf_stories, ereb). % <- chosen
tree(birdsong, 1, wolf_stories, heart).
tree(ithastry, 1, deep_mandaic, phost). % <- chosen
tree(illumination, 1, applebright_euphonies, mettle). % <- chosen
tree(the_bosk, 1, applebright_euphonies, ereb).
tree(hushery, 1, sights_and_sensations, wist).
tree(nyctodromy, 1, sights_and_sensations, fet). % <- chosen
tree(the_bosk, 1, weaving_and_knotworking, heart).
tree(birdsong, 1, weaving_and_knotworking, chor). % <- chosen

tree(illumination, 2, preliminal_meter, phost).
tree(illumination, 2, ragged_crossroads, phost).
tree(skolekosophy, 2, ragged_crossroads, shapt).

%% CRAFTING %%
craft(auroral_contemplations, lantern-5, eigengrau).
craft(inks_of_containment, winter-5, eigengrau).
craft(wolf_stories, moon-5, midnight_mark).
craft(wolf_stories, scale-5, bisclavrets_knot).
