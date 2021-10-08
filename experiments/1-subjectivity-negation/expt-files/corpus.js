// 40 most frequent noun-predicate combinations in the BNC

//[
//		{"Sentence": "box red", "Predicate": "red", "Noun": "box"},
//		{"Sentence": "box big", "Predicate": "big", "Noun": "box"}
//		]

var adjectives = _.shuffle([
		{"positive": "certain", "nPositive": "not certain", "negative": "uncertain", "nNegative": "not uncertain"},
		{"positive": "lucky", "nPositive": "not lucky", "negative": "unlucky", "nNegative": "not unlucky"},
		{"positive": "accurate", "nPositive": "not accurate", "negative": "inaccurate", "nNegative": "not inaccurate"},
		{"positive": "happy", "nPositive": "not happy", "negative": "unhappy", "nNegative": "not unhappy"},
		{"positive": "interesting", "nPositive": "not interesting", "negative": "uninteresting", "nNegative": "not uninteresting"},
		{"positive": "fair", "nPositive": "not fair", "negative": "unfair", "nNegative": "not unfair"},
		{"positive": "polite", "nPositive": "not polite", "negative": "impolite", "nNegative": "not impolite"},
		{"positive": "possible", "nPositive": "not possible", "negative": "impossible", "nNegative": "not impossible"},
		{"positive": "satisfactory", "nPositive": "not satisfactory", "negative": "unsatisfactory", "nNegative": "not unsatisfactory"},
		{"positive": "friendly", "nPositive": "not friendly", "negative": "unfriendly", "nNegative": "not unfriendly"},
		{"positive": "useful", "nPositive": "not useful", "negative": "useless", "nNegative": "not useless"},
		{"positive": "good", "nPositive": "not good", "negative": "bad", "nNegative": "not bad"},
		{"positive": "strong", "nPositive": "not strong", "negative": "weak", "nNegative": "not weak"},
		{"positive": "kind", "nPositive": "not kind", "negative": "mean", "nNegative": "not mean"},
		{"positive": "tall", "nPositive": "not tall", "negative": "short", "nNegative": "not short"},
		{"positive": "happy", "nPositive": "not happy", "negative": "sad", "nNegative": "not sad"},
		{"positive": "long", "nPositive": "not long", "negative": "short", "nNegative": "not short"},
		{"positive": "polite", "nPositive": "not polite", "negative": "rude", "nNegative": "not rude"},
		{"positive": "rich", "nPositive": "not rich", "negative": "poor", "nNegative": "not poor"},
		{"positive": "satisfactory", "nPositive": "not satisfactory", "negative": "frustrating", "nNegative": "not frustrating"}		
]);

// var nouns = [
// 		{"Noun":"apple", "NounClass":"food"},
// 		{"Noun":"banana", "NounClass":"food"},
// 		{"Noun":"carrot", "NounClass":"food"},
// 		{"Noun":"cheese", "NounClass":"food"},
// 		{"Noun":"tomato", "NounClass":"food"},								
// 		{"Noun":"chair", "NounClass":"furniture"},								
// 		{"Noun":"couch", "NounClass":"furniture"},								
// 		{"Noun":"fan", "NounClass":"furniture"},								
// 		{"Noun":"TV", "NounClass":"furniture"},								
// 		{"Noun":"desk", "NounClass":"furniture"}								
// ];

var stimuli =  adjectives

// function makeStims() {
// 	stims = [];

// 	for (var i=0; i<30; i++) {
// 		// noun = _.sample(nouns);
// 		stims.push(
// 			adjectives[i]
// 			// {
// 			// 	"Predicate":adjectives[i].Predicate,
// 			// 	"Class":adjectives[i].Class,				
// 			// 	// "Noun":noun.Noun,
// 			// 	// "NounClass":noun.NounClass
// 			// }
// 			);
// 		}
		
// 	return stims;
	
// }