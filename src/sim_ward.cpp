//ward.cpp
#include <Rcpp.h>
#include "sim_ward.h"
namespace transsim{
  ward_v2::ward_v2(unsigned int cap):capacity(cap){
    infected.reserve(capacity);
    susceptible.reserve(capacity);
  };
  bool ward_v2::empty(){return infected.empty() && susceptible.empty();}
  bool ward_v2::full(){return size()>=capacity;}
  size_type ward_v2::size(){return infected.size()+susceptible.size();}
  size_type ward_v2::maxSize(){return capacity;}
  size_type ward_v2::nInfected(){return infected.size();}
  size_type ward_v2::nSusceptible(){return susceptible.size();}
  void ward_v2::dischargeRandom(mytime at){
    unsigned int n=size();
    unsigned int td = static_cast<unsigned int>(Rcpp::runif(1,0,n)[0]);
    if(td<infected.size()){
      infected[td]->setDischarge(at);
      infected.erase(infected.begin()+td);
    } else {
      td -= infected.size();
      susceptible[td]->setDischarge(at);
      susceptible.erase(susceptible.begin()+td);
    }
  }
  void ward_v2::infectRandom(mytime at){
    unsigned int n=susceptible.size();
    int i = trunc(Rcpp::runif(1,0,n)[0]);
    pp patient2infect = susceptible[i];
    susceptible.erase(susceptible.begin()+i);
    patient2infect->infect(at);
    infected.push_back(patient2infect);
  }
  void ward_v2::add(pp patientPtr){
    if(patientPtr->isInfected()) infected.push_back(patientPtr);
    else susceptible.push_back(patientPtr);
  }
  void ward_v2::testSomeone(mytime at, double fn=0, double fp=0){
    unsigned int n=size();
    unsigned int i = trunc(Rcpp::runif(1L,0L,n)[0]);
    if(i < infected.size()){
      infected[i]->doTest(at,fn,fp);
    } else {
      i -= infected.size();
      susceptible[i]->doTest(at,fn,fp);
    }
  }
}
