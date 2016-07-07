var app = angular.module('uniChance', ['autocomplete', 'smoothScroll']);

app.factory('statsService', function($http, $q) {
	return {
		getNumbers : function(uniName, subjectName, theirScore) {
			var pointAcceptPromise = $http.get('/points/acceptances/' + theirScore.toString());
			var pointApplicPromise = $http.get('/points/applicants/' + theirScore.toString());

			var uriStr = encodeURIComponent(uniName) + '/' + encodeURIComponent(subjectName);
			var subjAcceptPromise = $http.get('/subjects/acceptances/' + uriStr);
			var subjApplicPromise = $http.get('/subjects/applicants/' + uriStr);

			return $q.all([pointAcceptPromise, pointApplicPromise, subjAcceptPromise, subjApplicPromise]).then(function(vals) {
				var accP = vals[0].data;
				var appP = vals[1].data;
				var accS = vals[2].data;
				var appS = vals[3].data;

				// Loop through years
				var scoreStat = 0;
				var nyears = accP.length;
				for (var i = 0; i < nyears; i++) {
					// Older years ascribed less statistical significance
					var weight = i + 1;
					var percentage = weight*(accP[i].num / appP[i].num);
					scoreStat += percentage
				}
				scoreStat /= 0.5*nyears*(nyears + 1); // Normalise
				scoreStat *= 100; // Percentage

				// Loop through subject years (different number hence diff loop. Could be better)
				var subjStat = 0;
				var nyears = accS.length;
				for (var i = 0; i < nyears; i++) {
					// Older years ascribed less statistical significance
					var weight = i + 1;
					var percentage = weight*(accS[i].num / appS[i].num);
					subjStat += percentage
				}
				subjStat /= 0.5*nyears*(nyears + 1); // Normalise
				subjStat *= 100; // Percentage

				// Arbitrary linear combination capped at 95%
				var combinedStat = 0.95*(0.6*scoreStat + 0.4*subjStat);

				return { subj : subjStat, score : scoreStat, combined : combinedStat };
			});
		},

		getSimilarUnis : function(uniName) {
			var uniPromise = $http.get('/leaguetable/' + encodeURIComponent(uniName));
			return uniPromise.then(function(unis) {
				return unis;
			});
		}
	}
});

app.controller('mainController', [
	'$scope', '$http', '$q', 'statsService',
	function($scope, $http, $q, statsService) {
		$scope.subjects = [];
		$scope.uni = "";
		$scope.combinedStat = 0; $scope.scoreStat = 0; $scope.subjStat = 0;
		$scope.gradeTypes = [{name: "A*", val: 6},
												 {name: "A", val: 5},
												 {name: "B", val: 4},
												 {name: "C", val: 3},
												 {name: "D", val: 2},
												 {name: "E", val: 1}];

		$scope.doCalc = function() {
			// First do the basic stats from UCAS data
			var score = $scope.grade1 + $scope.grade2 + $scope.grade3;
			statsService.getNumbers($scope.uni, $scope.subject, score).then(function(stats) {
				$scope.combinedStat = stats.combined;
				$scope.scoreStat = stats.score;
				$scope.subjStat = stats.subj;
				return true;
			});

			// Get similar unis (from league table). Async.
			var similarUnis = [];
			statsService.getSimilarUnis($scope.uni).then(function(unis) {
				similarUnis = unis;
				var extraStatPromises = [];
				unis.data.forEach(function(uni) {
					var thisPromise = statsService.getNumbers(uni.uni, $scope.subject, score);
					extraStatPromises.push(thisPromise);
				});
				return extraStatPromises;
			}).then(function(promises) {
				return $q.all(promises);
			}).then(function(allResults) {
				$scope.similarUnis = [];
				console.log(similarUnis);
				for(var i = 0; i < allResults.length; i++) {
					$scope.similarUnis.push({ name : similarUnis.data[i].uni,
																		stat : allResults[i].combined });
				}
			});
		}

		$scope.loadData = function() {
			$http.get('/subjects').success(function(ret) {
				$scope.subjects = angular.copy(ret);
			});

			$http.get('/unis').success(function(ret) {
				$scope.unis = angular.copy(ret);
			});
			return true;
		};
	}
]);