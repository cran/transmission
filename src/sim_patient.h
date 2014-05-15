#pragma once
#ifndef __patient_hpp
#define __patient_hpp
#include "sim_event.h"
#include "sim_test.h"
#include <vector>
#include <map>
#include <string>
#include <Rcpp.h>

using namespace std;

namespace transsim{

/*! Patient class
 *
 */
class patient{
  private:
    patient & operator=(const patient & lhs);
  protected:
    event admit;
    event discharge;
    event infection;
    std::vector<test> tests;
  public:
    patient(mytime a):admit(a){}
    patient(mytime a, mytime d):admit(a),discharge(d){}
    patient(mytime a, mytime d, mytime i):admit(a),discharge(d),infection(i){}
    bool isInfected(mytime at){
      //if(infection.unknown()) return false;
      return infection < at;
    }
    bool isInfected(){return infection.known();}
    bool isImported(){return infection==admit;}
    void infect(mytime at){infection = at;}
    void disinfect(){infection.clear();} 
    void setDischarge(mytime d){discharge=d;}
    void addTest(test t){tests.push_back(t);}
    void doTest(mytime at, double fn=0, double fp=0){
      bool infected=isInfected(at);
      bool result = infected;
      if(infected){
        if(fn>0 && Rcpp::runif(1,0,1)[0]<fn) result=false;
      } else {
        if(fp>0 && Rcpp::runif(1,0,1)[0]<fp) result=true;
      }
      addTest(test(at,result)); 
    }
    mytime admissionTime(){return admit.when();}
    mytime dischargeTime(){return discharge.when();}
    mytime infectionTime(){return infection.when();}
    unsigned int ntests(){return tests.size();} 
    std::vector<test>::const_iterator testsBegin(){return tests.begin();}
    std::vector<test>::const_iterator testsEnd(){return tests.end();}
    class byAdmit{
      private:
        bool r;
      public:
        byAdmit(const bool reverse=false):r(reverse){}
        bool operator()(const patient& lhs, const patient& rhs)const{
          if(r) return (lhs.admit>rhs.admit);
          else  return (lhs.admit<rhs.admit);
        }
        bool operator()(const patient*const lhs, const patient*const rhs)const{
          if(r) return ((*lhs).admit>(*rhs).admit);
          else  return ((*lhs).admit<(*rhs).admit);
        }
    };
    class byDischarge{
      private:
        bool r;
      public:
        byDischarge(const bool reverse=false):r(reverse){}
        bool operator()(const patient& lhs, const patient& rhs)const{
          if(lhs.discharge.unknown() && rhs.discharge.unknown()) return false;
          if(r) return (lhs.discharge>rhs.discharge);
          else  return (lhs.discharge<rhs.discharge);
        }
        bool operator()(const patient*const lhs, const patient*const rhs)const{
          if((*lhs).discharge.unknown() && (*rhs).discharge.unknown()) return false;
          if(r) return ((*lhs).discharge>(*rhs).discharge);
          else  return ((*lhs).discharge<(*rhs).discharge);
        }
        
    };
    class byInfection{
      private:
        bool r;
      public:
        byInfection(const bool reverse=false):r(reverse){}
        bool operator()(const patient& lhs, const patient& rhs){
          if(lhs.infection.unknown() && rhs.infection.unknown()) return false;
          if(r) return (lhs.infection>rhs.infection);
          else  return (lhs.infection<rhs.infection);
        }
        bool operator()(const patient* lhs, const patient* rhs){
          if((*lhs).infection.unknown() && (*rhs).infection.unknown()) return false;
          if(r) return ((*lhs).infection>(*rhs).infection);
          else  return ((*lhs).infection<(*rhs).infection);
        }
    };
};
}

#endif // __patient_hpp
