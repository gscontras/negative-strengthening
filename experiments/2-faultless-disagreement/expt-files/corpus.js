// 40 most frequent noun-predicate combinations in the BNC

//[
//		{"Sentence": "box red", "Predicate": "red", "Noun": "box"},
//		{"Sentence": "box big", "Predicate": "big", "Noun": "box"}
//		]

// var adjectives = _.shuffle([
// 		{"Predicate":"red", "Class":"color"},
// 		{"Predicate":"yellow", "Class":"color"},
// 		{"Predicate":"green", "Class":"color"},
// 		{"Predicate":"blue", "Class":"color"},
// 		{"Predicate":"purple", "Class":"color"},
// 		{"Predicate":"brown", "Class":"color"},											
// 		{"Predicate":"big", "Class":"size"},
// 		{"Predicate":"small", "Class":"size"},					
// 		{"Predicate":"huge", "Class":"size"},					
// 		{"Predicate":"tiny", "Class":"size"},					
// 		{"Predicate":"short", "Class":"size"},					
// 		{"Predicate":"long", "Class":"size"},							
// 		{"Predicate":"wooden", "Class":"material"},
// 		{"Predicate":"plastic", "Class":"material"},
// 		{"Predicate":"metal", "Class":"material"},
// 		{"Predicate":"smooth", "Class":"texture"},
// 		{"Predicate":"hard", "Class":"texture"},
// 		{"Predicate":"soft", "Class":"texture"},
// 		{"Predicate":"old", "Class":"age"},
// 		{"Predicate":"new", "Class":"age"},
// 		{"Predicate":"rotten", "Class":"age"},
// 		{"Predicate":"fresh", "Class":"age"},
// 		{"Predicate":"good", "Class":"quality"},
// 		{"Predicate":"bad", "Class":"quality"},
// 		{"Predicate":"round", "Class":"shape"},						
// 		{"Predicate":"square", "Class":"shape"}
// ]);


var adjectives = _.shuffle([
		{"positive": "certain", "NPositive": "not certain", "negative": "uncertain", "NNegative": "not uncertain"},
		{"positive": "lucky", "NPositive": "not lucky", "negative": "unlucky", "NNegative": "not unlucky"},
		{"positive": "accurate", "NPositive": "not accurate", "negative": "inaccurate", "NNegative": "not inaccurate"},
		{"positive": "happy", "NPositive": "not happy", "negative": "unhappy", "NNegative": "not unhappy"},
		{"positive": "interesting", "NPositive": "not interesting", "negative": "uninteresting", "NNegative": "not uninteresting"},
		{"positive": "fair", "NPositive": "not fair", "negative": "unfair", "NNegative": "not unfair"},
		{"positive": "polite", "NPositive": "not polite", "negative": "impolite", "NNegative": "not impolite"},
		{"positive": "possible", "NPositive": "not possible", "negative": "impossible", "NNegative": "not impossible"},
		{"positive": "satisfactory", "NPositive": "not satisfactory", "negative": "unsatisfactory", "NNegative": "not unsatisfactory"},
		{"positive": "friendly", "NPositive": "not friendly", "negative": "unfriendly", "NNegative": "not unfriendly"},
		{"positive": "useful", "NPositive": "not useful", "negative": "useless", "NNegative": "not useless"},
		{"positive": "good", "NPositive": "not good", "negative": "bad", "NNegative": "not bad"},
		{"positive": "strong", "NPositive": "not strong", "negative": "weak", "NNegative": "not weak"},
		{"positive": "kind", "NPositive": "not kind", "negative": "mean", "NNegative": "not mean"},
		{"positive": "tall", "NPositive": "not tall", "negative": "short", "NNegative": "not short"},
		{"positive": "happy", "NPositive": "not happy", "negative": "sad", "NNegative": "not sad"},
		{"positive": "long", "NPositive": "not long", "negative": "short", "NNegative": "not short"},
		{"positive": "polite", "NPositive": "not polite", "negative": "rude", "NNegative": "not rude"},
		{"positive": "rich", "NPositive": "not rich", "negative": "poor", "NNegative": "not poor"},
		{"positive": "satisfactory", "NPositive": "not satisfactory", "negative": "frustrating", "NNegative": "not frustrating"}		
]);

var nouns = [
		{"Noun":"government", "NounClass":"organization"},
		{"Noun":"design", "NounClass":"organization"},
		{"Noun":"country", "NounClass":"organization"},
		{"Noun":"dorm", "NounClass":"organization"},
		{"Noun":"university", "NounClass":"organization"},								
		{"Noun":"team", "NounClass":"people"},								
		{"Noun":"person", "NounClass":"people"},								
		{"Noun":"man", "NounClass":"people"},								
		{"Noun":"woman", "NounClass":"people"},								
		{"Noun":"group", "NounClass":"people"}								
];

var stimuli =  makeStims();

function makeStims() {
	stims = [];

	for (var i=0; i<adjectives.length; i++) {
		noun = _.sample(nouns);
		stims.push(
			{
				"Predicate":adjectives[i],			
				"Noun":noun.Noun,
				"NounClass":noun.NounClass
			}
			);
		}
		
	return stims;
	
}