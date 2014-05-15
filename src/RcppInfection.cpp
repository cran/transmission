/************************************************************ 
* RcppInfection.cc
* Copyright 2011 Andrew Redd
* 12/15/2011
*
* Part of transmission package
* Description
* Provides interface classed for c++ code for
* fitting the continuous time infectious disease models
* fit on individual level data.
************************************************************/
#include <Rcpp.h>
#include <vector>
#include <string>
#include <sstream>
#include <exception>
#include "infection.h"

using namespace Rcpp;
using namespace std;
using namespace infection;

namespace continuous{
double Runif(){
  return Rcpp::runif(1, 0.0, 1.0)[0];
}

class RInfectionMCMC : public InfectionMCMC{
private:
public:
  RInfectionMCMC() : InfectionMCMC(){
    InfectionMCMC::setRandomUniform(Runif);
  }
  RInfectionMCMC(bool use_slope) : InfectionMCMC(use_slope){
    InfectionMCMC::setRandomUniform(Runif);
  }
  void LoadDataByEvents(const DataFrame &events){
    NumericVector eventtime = events[0];
    IntegerVector eventpid  = events[1];
    IntegerVector eventtype = events[2];
    int N = eventtime.length();
    for(int i=0; i<N; i++) {
      double time = eventtime[i];
      int    pid  = eventpid[i],
             type = eventtype[i];
      addEvent(time, pid, type);
    }
    h->finalize();
  }
  void LoadData(const DataFrame& patients, const DataFrame& tests) {
    bool hasInfection = patients.length() >= 4;

    { //Load patients
    IntegerVector id = patients[0];
    NumericVector admission = patients[1];
    NumericVector discharge = patients[2];
    NumericVector infection;
    LogicalVector neverInfected;
    if(hasInfection) 
    {
      infection = patients[3];
      neverInfected = is_na(infection);
    }
    
    int numPatients = admission.length();
    
    int idxPatient;
    for(idxPatient = 0; idxPatient < numPatients; idxPatient++)
    {
      addEvent(admission[idxPatient], id[idxPatient], Event::admission);
      addEvent(discharge[idxPatient], id[idxPatient], Event::discharge);
      if(hasInfection && !(neverInfected[idxPatient]))
        addEvent(infection[idxPatient], id[idxPatient], Event::infection);
    }
    }
    { // Load Tests
    IntegerVector pid = tests[0];
    NumericVector testtime = tests[1];
    LogicalVector testresult = tests[2];
    int numTests = pid.length();
    for(int idxTest=0; idxTest<numTests; idxTest++) {
      int result = (testresult[idxTest]) ? (Event::postest) : (Event::negtest);
      addEvent(testtime[idxTest], pid[idxTest], result);
    }
    }
    h->finalize();
  }
  void LoadDataByList(const List& PTlist){
    // const DataFrame patients = Rcpp::DataFrame_Impl<PreserveStorage>(PTlist[0]);  // Assignment due to a new requirement that it be compilable by clang.
    // const DataFrame tests    = Rcpp::DataFrame_Impl<PreserveStorage>(PTlist[1]);
    LoadData( Rcpp::DataFrame_Impl<PreserveStorage>(PTlist[0])
            , Rcpp::DataFrame_Impl<PreserveStorage>(PTlist[1])
            );
  }
  
  
//*/
  SEXP run(int nsims){return run_core(nsims, false);}
  SEXP runll(int nsims, bool includell){return run_core(nsims, includell);}
  SEXP run_core(int nsims, bool includell=false) {
    RNGScope RNG;
    BEGIN_RCPP

    IntegerVector rInum(nsims);
    //IntegerVector rns(nsims);
    NumericVector rTransmission(nsims);
    NumericVector rImportation(nsims);
    NumericVector rFalseNeg(nsims);
    NumericVector rFalsePos(nsims);
    NumericVector rLL(includell?nsims:0);
    NumericVector rSlope((c->using_slope)?nsims:0);

    // Run Simulation
    for (int isim=0; isim<nsims; isim++)
    {
        nextSample();
        rInum(isim)         = nSamples();
        rTransmission(isim) = c->getTransmission();
        rImportation(isim)  = c->getImportation();
        rFalsePos(isim)     = c->getFalsePos();
        rFalseNeg(isim)     = c->getFalseNeg();
      if(c->using_slope)
        rSlope              = c->getSlope();
      if(includell)
        rLL(isim)           = getLikelihood();
    }
    
    List results = List::create(
        _(".n")            = rInum
      , _("transmission")  = rTransmission
      , _("importation")   = rImportation
      , _("false.neg")     = rFalseNeg
      , _("false.pos")     = rFalsePos
      );
    
    if(includell) {
        results["loglik"] = rLL;
    } 
    if(c->using_slope) {
        results["slope"] = rSlope;
    }
    return DataFrame(results);
    END_RCPP
  }
  SEXP events() {
    BEGIN_RCPP
    int nevents = h->length();
    NumericVector time(nevents);
    IntegerVector pid(nevents);
    IntegerVector type(nevents);

    for (int i=0; i<nevents; i++) {  // i=1 to skip marker event
      Event* pe = h->get(i);
      time[i] = pe->time();
      pid[i]  = pe->patient();
      type[i] = pe->type();
    }

    return DataFrame::create(
      _("time")  = time,
      _("pid")   = pid,
      _("event") = type);
    END_RCPP
  }
  int nEvents(){ return h->length();}
  int nPatients(){ return h->nPatients() - 1;}

  double getTransmission(         ){return  InfectionMCMC::getTransmission(  );}
  void   setTransmission(double in){        InfectionMCMC::setTransmission(in);}
  double getImportation (         ){return  InfectionMCMC::getImportation (  );}
  void   setImportation (double in){        InfectionMCMC::setImportation (in);}
  double getFalseNeg    (         ){return  InfectionMCMC::getFalseNeg    (  );}
  void   setFalseNeg    (double in){        InfectionMCMC::setFalseNeg    (in);}
  double getFalsePos    (         ){return  InfectionMCMC::getFalsePos    (  );}
  void   setFalsePos    (double in){        InfectionMCMC::setFalsePos    (in);}
  double getLikelihood(){return InfectionMCMC::getLikelihood();}
  int nSamples(){ return InfectionMCMC::nSamples();}
  // SEXP getDoFlags() {
    // return LogicalVector::create(
        // _["States"]      = dos  != 0
      // , _["Transmission"]= dot  != 0 
      // , _["Importation"] = doi  != 0
      // , _["FalseNeg"]    = dofn != 0
      // , _["FalsePos"]    = dofp != 0
    // );
  // }
  void setDoStates(bool yn){dos = yn;}
  void setDoTransmission(bool yn){dot = yn;}
  void setDoTransSlope(bool yn){dot = yn;}
  void setDoImportation(bool yn){doi = yn;}
  void setDoFN(bool yn){dofn = yn;}
  void setDoFP(bool yn){dofp = yn;}
  bool getDoStates()      {return dos ;}
  bool getDoTransmission(){return dot ;}
  bool getDoImportation() {return doi ;}
  bool getDoFN()          {return dofn;}
  bool getDoFP()          {return dofp;}
  bool getDoSlope()       {return dot1;}
  void setDoSlope(bool in){dot1=in;}
  bool getUsingSlope()    {return c->using_slope;}
};
}

using namespace continuous;

RCPP_MODULE(continuous){
class_<continuous::RInfectionMCMC>("cont.inf.model")
    .constructor("default constructor")
    .constructor<bool>("Constructor for a slope based model.")
    .method<SEXP, int>("run", &continuous::RInfectionMCMC::run, "run the MCMC chain")
    .method<SEXP, int>("runll", &continuous::RInfectionMCMC::runll, "run the MCMC chain")
    .method("load_by_events", &continuous::RInfectionMCMC::LoadDataByEvents, "Load data into the object from a data frame of events.")
    .method("load", &continuous::RInfectionMCMC::LoadData, "Load data into the object by patient and test data frames", &yes)
    .method("load_by_list", &continuous::RInfectionMCMC::LoadDataByList, "Load data into the object from a list with patient and test data frames", &yes)
    // methods for testing
    .property("events", &continuous::RInfectionMCMC::events,
            "retrieve the internal data of events")
    .property("nEvents", &continuous::RInfectionMCMC::nEvents,
            "retrieve the number of events stored in the history.")
    .property("nPatients", &continuous::RInfectionMCMC::nPatients,
            "retrieve the number of patients stored in the history.")
    .property("using_slope", &continuous::RInfectionMCMC::getUsingSlope)
    
    // Fitting Flags
           
    // sevaral aliases for model parameters.
     .property("Transmission", &continuous::RInfectionMCMC::getTransmission,
                               &continuous::RInfectionMCMC::setTransmission)
     .property("transmission", &continuous::RInfectionMCMC::getTransmission,
                               &continuous::RInfectionMCMC::setTransmission,
                               "alias for Transmission")
     .property("Importation" , &continuous::RInfectionMCMC::getImportation,
                               &continuous::RInfectionMCMC::setImportation)
     .property("importation" , &continuous::RInfectionMCMC::getImportation,
                               &continuous::RInfectionMCMC::setImportation,
                               "Alias for Imporation")
     .property("FalseNeg"    , &continuous::RInfectionMCMC::getFalseNeg,
                               &continuous::RInfectionMCMC::setFalseNeg)
     .property("false.neg"   , &continuous::RInfectionMCMC::getFalseNeg,
                               &continuous::RInfectionMCMC::setFalseNeg,
                               "Alias for FalseNeg")
     .property("fn"          , &continuous::RInfectionMCMC::getFalseNeg,
                               &continuous::RInfectionMCMC::setFalseNeg,
                               "Alias for FalseNeg")
     .property("FalsePos"    , &continuous::RInfectionMCMC::getFalsePos,
                               &continuous::RInfectionMCMC::setFalsePos)
     .property("false.pos"   , &continuous::RInfectionMCMC::getFalsePos,
                               &continuous::RInfectionMCMC::setFalsePos,
                               "Alias for FalsePos")
     .property("fp"          , &continuous::RInfectionMCMC::getFalsePos,
                               &continuous::RInfectionMCMC::setFalsePos,
                               "Alias for FalsePos")
     .property("logLik"      , &continuous::RInfectionMCMC::getLikelihood,
                               "Log likelihoods for he current states")
     .property("nPatients"   , &continuous::RInfectionMCMC::nPatients,
                               "Number of patients in population.")
     .property("nSamples"    , &continuous::RInfectionMCMC::nSamples,
                               "Number of patients in population.")
     .property("nSamples"    , &continuous::RInfectionMCMC::nSamples,
                               "Number of patients in population.")
     .property("doStates"    , &continuous::RInfectionMCMC::getDoStates,
                               &continuous::RInfectionMCMC::setDoStates,
                               "")
     .property("doTransmission",&continuous::RInfectionMCMC::getDoTransmission,
                                &continuous::RInfectionMCMC::setDoTransmission,
                               "")
     .property("doImportation"   ,  &continuous::RInfectionMCMC::getDoImportation,
                                    &continuous::RInfectionMCMC::setDoImportation,
                               "")
     .property("doFN"   ,  &continuous::RInfectionMCMC::getDoFN,
                           &continuous::RInfectionMCMC::setDoFN,
                               "")
     .property("doFP"   ,  &continuous::RInfectionMCMC::getDoFP,
                           &continuous::RInfectionMCMC::setDoFP,
                               "")
     .property("doSlope"   ,  &continuous::RInfectionMCMC::getDoSlope,
                           &continuous::RInfectionMCMC::setDoSlope,
                               "")
     //.property("doFlags", &continuous::RInfectionMCMC::getDoFlags)
    ;
//*/
}
