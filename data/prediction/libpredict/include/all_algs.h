#pragma once


typedef double(*objCalFunctionType)(double ** X_dat, double * Y_dat, int d, int n, void * modelPara, void * additionalStuff);
typedef void(*gradientStepFunctionType) (double * x, double y, double *gradAns, int d, bool cumu, void * modelPara, void * additionalStuff);
typedef void(*trainModelFunctionType) (double ** X_dat, double* Y_dat, int d, int n, double avg, void * modelPara, void * additionalStuff);
typedef void(*predictNextFunctionType) (double * x, double * predictY, int d, void * modelPara, void * additionalStuff);


/*
General useful functions.
*/
double sqr(double a);

void  getLoss(double * regret, double * serve, double * predict, int length, int penalty);

double getRandFloat();

void writeRandomDouble(double * w, int d);

void fullGradient(double ** X_dat, double * Y_dat, double * gradAns, int d, int n,
		  gradientStepFunctionType gradientStep, void * modelPara, void * additionalStuff);

double inner(double *w, double * x, int d);

void proximalUpdate(double * w, int d, double thres);

/*
UniVR: a variance reduced SGD algorithm.
Currently the function below supports d-dimensional model only.
In order to support neural nets, I wrote another special one.
*/
void uniVR(double ** X_dat, double * Y_dat, int d, int n,
	   gradientStepFunctionType gradientStep, objCalFunctionType objCal, void * modelPara, void * additionalStuff);

/*
Naive predicting the last value.
There is no training
*/
void lastOnePrediction(double * x, double * predictY, int d, void * modelPara, void * additionalStuff);

/*
Linear Regression Code:
*/
double linearRegressionObjCal(double ** X_dat, double * Y_dat, int d, int n, void * modelPara, void * additionalStuff);

void linearRegressionGradientStep(double *x, double y, double * gradAns, int d, bool cumu, void * modelPara, void * additionalStuff);

void linearRegressionTrain(double ** X_dat, double * Y_dat, int d, int n, double avg, void * modelPara, void * additionalStuff);

void linearRegressionPredict(double * x, double * predictY, int d, void * modelPara, void * additionalStuff);



//Train a model
void trainModel(trainModelFunctionType trainMethod,
		double ** serve, int numOfFeature, int length, void * modelPara, void * additionalStuff);

double predictOneModel(predictNextFunctionType predictMethod, double * serve, int numOfFeature, int length, void * modelPara, void * addtionalStuff);

