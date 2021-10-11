// 40 most frequent noun-predicate combinations in the BNC

//[
//		{"Sentence": "box red", "Predicate": "red", "Noun": "box"},
//		{"Sentence": "box big", "Predicate": "big", "Noun": "box"}
//		]

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