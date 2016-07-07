var express = require('express');
var router = express.Router();
var assert = require('assert');

/* GET home page. */
router.get('/', function(req, res, next) {
  res.render('index', { title: 'Express' });
});

module.exports = router;

var distinctLevels = function(colname, req, res) {
	var db = req.db;
	var subjectApplications = db.collection('subjectApplications');
	subjectApplications.distinct(colname)
	                   .then(function(subjects) {
	                      res.send(subjects);
	                   });	
};

// Gets list of subjects
router.get('/subjects', function(req, res) {
	distinctLevels("subject", req, res);
});

// Gets list of unis
router.get('/unis', function(req, res) {
	distinctLevels("uni", req, res);
});

// Parameter for selecting by A-level score
router.param('score', function(req, res, next, theirScore) {
	req.collection.find({ "score" :  parseInt(theirScore) }, 'num')
							.then(function(result) {
								req.scores = result;
								next();
							});
});

router.get('/points/:type(applicants|acceptances)/:score', function(req, res) {
	res.json(req.scores);
});

router.get('/subjects/:type(applicants|acceptances)/:uni/:subject', function(req, res) {
	var uni = req.params.uni;
	var subject = req.params.subject;

	req.collection.find({ "uni" : uni, "subject" : subject}, 'num')
								.then(function(result) {
									res.json(result);
								});
});

router.get('/leaguetable/:uni', function(req, res) {
	var db = req.db;
	var uni = req.params.uni;
	var rank = -1;
	var collection = db.get('guardianTable');

	// Get our rank
	collection.findOne({ "uni" : uni }, 'rank')
		.then(function(ret) {
			if (ret) {
				rank = parseInt(ret.rank);
				return collection.find({ $and : [ {"rank" : { $gt : (rank - 3), $lte : (rank + 2) } },
																 					{"uni" : { $ne : uni} } ] });
			} else {
				res.json(null);
			}
		}).then(function(nearUnis) {
			res.json(nearUnis);
		});
});

