#include <Rcpp.h>
#include <list>
#include <vector>
#include <map>
#include <queue>
#include <assert.h>
#include <exception>
#include "sim_event.h"
#include "sim_patient.h"
#include "sim_ward.h"
#include "sim_test.h"

using namespace std;
using namespace Rcpp;
namespace transsim{
//event methods
const mytime time_unknown = std::numeric_limits<mytime>::max();
bool known(const mytime t){ return t!=time_unknown;}
bool unknown(const mytime t){ return t==time_unknown;}
//helper funcetions
double uniform_transformation(double x, mytime t){return x;}

class sim_v2{
  private:
    list<patient*> all;
    mytime end;
    double lambda;
    double importation;
    double test_rate;
    double false_negative_rate;
    double false_positive_rate;
    double mean_stay;
    double balance;
    ward_v2 current;
    enum possibleStep{sadmit, sdischarge, sinfect, stest, snone}; 
  public:
    sim_v2(unsigned int cap): end(0), lambda(0), importation(0), test_rate(0), false_negative_rate(0), false_positive_rate(0), mean_stay(0), balance(0), current(cap){}     
    ~sim_v2(){
      for(std::list<patient*>::iterator patient=all.begin(); patient!= all.end(); patient++){
        delete *patient;
        *patient=NULL;
      }
    }
    double get_lambda(){return lambda;}
    void set_lambda(double x){ lambda=x;}
    double get_importation(){return importation;}
    void set_importation(double x){ importation=x;}
    double get_test_rate(){return test_rate;}
    void set_test_rate(double x){ test_rate=x;}
    double get_false_negative_rate(){return false_negative_rate;}
    void set_false_negative_rate(double x){ false_negative_rate=x;}
    double get_false_positive_rate(){return false_positive_rate;}
    void set_false_positive_rate(double x){ false_positive_rate=x;}
    double get_mean_stay(){return mean_stay;}
    void set_mean_stay(double x){ mean_stay=x;}
    double get_balance(){return balance;}
    void set_balance(double x){ balance=x;}
    unsigned int get_capacity(){ return current.maxSize();}
    mytime get_length(){return end;}
    unsigned int run(mytime length){
      RNGScope scope;
      if(mean_stay<=0) throw runtime_error("mean_stay not a positive value.");
      double drate = 1/mean_stay;
      double arate = drate*balance*current.maxSize();
      mytime now=end;
      end += length;
      while(now<end){
        possibleStep step=snone;
        mytime 
          t=0,
          next=time_unknown;
        
        if(lambda>0 && current.nSusceptible()!=0 && current.nInfected()!=0){
          next=t=rexp(1, lambda * current.nSusceptible() * current.nInfected() )[0];
          step=sinfect;
        }
        if(!current.full()){
          t=rexp(1, arate)[0];
          if(t<next){
            next=t;
            step=sadmit;
          }
        }
        if(!current.empty()){
          t=rexp(1,drate*current.size())[0];
          if(t<next){
            next=t;
            step=sdischarge;
          }
        }
        if(!current.empty() && test_rate>0){
          t=rexp(1,test_rate)[0];
          if(t<next){
            next=t;
            step=stest;
          }
        }
        now+=next;
        switch(step){
          case sadmit:
            {
              pp pat = new patient(now);
              if(runif(1,0,1)[0]<importation) pat->infect(now);
              current.add(pat);
              all.push_back(pat);
            }
            break;
          case sdischarge:
            current.dischargeRandom(now);
            break;
          case sinfect:
            current.infectRandom(now);
            break;
          case stest:
            current.testSomeone(now, false_negative_rate, false_positive_rate);
            break;
          default:
            break;
        }
      }
      end = now;
      return all.size();
    }
    Rcpp::List getData(){
      //int n = allPatients.size();
      std::vector<mytime> admit;
      std::vector<mytime> discharge;
      std::vector<mytime> infection;
      std::vector<bool> infected;

      unsigned int tid=0, pid=0;
      std::vector<int> pidl;
      std::vector<mytime> testtimes;
      std::vector<bool> results;

      for(std::list<patient*>::const_iterator patient=all.begin(); patient!= all.end(); patient++){
        admit.push_back((*patient)->admissionTime());
        mytime d= (*patient)->dischargeTime();
        discharge.push_back((d==time_unknown)?NA_REAL:d);
        mytime i= (*patient)->infectionTime();
        infection.push_back((i==time_unknown)?NA_REAL:i);
        pid++; // yes this is supposed to be here so R indexes are correct, not C offset.
        //Swabs
        for(std::vector<test>::const_iterator s=(*patient)->testsBegin(); s!=(*patient)->testsEnd();s++){
          pidl.push_back(pid);
          testtimes.push_back((*s).when());
          results.push_back((*s).isPositive());
          tid++;
        }
      }
      NumericVector  Radmit = wrap(admit);
      NumericVector  Rdischarge = wrap(discharge);
      NumericVector  Rinfection = wrap(infection);
      DataFrame patients = DataFrame::create(
        _["admit"] = Radmit,
        _["discharge"] = Rdischarge,
        _["infection"] = Rinfection);

      IntegerVector Rpid = wrap(pidl);
      NumericVector Rtesttimes = wrap(testtimes);
      LogicalVector Rresults = wrap(results);
      DataFrame tests = DataFrame::create(
        _["pid"] = Rpid, //wrap(pidl),
        _["time"] = Rtesttimes,//wrap(testtimes),
        _["result"]= Rresults);//wrap(results));
      return List::create(
        _["patients"] = patients,
        _["tests"] = tests);
    }
  };
};

RCPP_MODULE(sim){
  class_<transsim::sim_v2>("sim2")
    .constructor<int>("Setup simulation with a given capacity.")
    .method("run",&transsim::sim_v2::run, "run for longer.")
    .method("getData", &transsim::sim_v2::getData, "gets the Data from the simulation")
    .property("transmission", &transsim::sim_v2::get_lambda, &transsim::sim_v2::set_lambda,"Transmission as a hazard.")
    .property("importation", &transsim::sim_v2::get_importation, &transsim::sim_v2::set_importation,"Importation as a probability")
    .property("test_rate", &transsim::sim_v2::get_test_rate, &transsim::sim_v2::set_test_rate,"Rate of testing in n/day.")
    .property("balance", &transsim::sim_v2::get_balance, &transsim::sim_v2::set_balance,"The balance between admission and discharges.  This is approximatly the percent of full for the ward.")
    .property("fn", &transsim::sim_v2::get_false_negative_rate, &transsim::sim_v2::set_false_negative_rate,"probability of a false negative")
    .property("fp", &transsim::sim_v2::get_false_positive_rate, &transsim::sim_v2::set_false_positive_rate,"probability of a false positive")
    .property("mean_stay", &transsim::sim_v2::get_mean_stay, &transsim::sim_v2::set_mean_stay,"Mean number of days a patient stays in the hospital.")
    .property("capacity",&transsim::sim_v2::get_capacity, "Retrieves capacity for the ward.")
    .property("length", &transsim::sim_v2::get_length,"get the length of the simulation.")
    ;
  
}

