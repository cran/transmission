#include <exception>
#include <map>
#include <list>
#include <assert.h>
#include "sim_event.h"
#include "sim_patient.h"
#include <string>

using namespace std;
namespace transsim{
typedef patient* pp;
typedef std::map<int,pp> ppmap;
typedef ppmap::size_type size_type;
typedef ppmap::iterator ppi;
typedef ppmap::const_iterator ppci;

/*! Represents a ward
 *
 */
class ward{
 private:
    ppmap currentPatients;
    size_type capacity;
    int ninfected;
    bool ninfected_valid;
 protected:
    ppmap::iterator byIndex(unsigned int i){
      if(empty()) throw empty_error();
      if(i>currentPatients.size()) throw range_error("patient index out of bounds");
      ppi it = currentPatients.begin();
      while(i--)it++;
      return it;
    }
 public:
    ward(size_type maxSize):capacity(maxSize), ninfected(0), ninfected_valid(true){
      if(capacity<=0) throw runtime_error("Cannot create a ward with no capacity");
    }
    class toobig:std::exception{};
    class empty_error:std::runtime_error{
      public: empty_error():runtime_error("Ward is empty."){};
    };
    const size_type size(){return currentPatients.size();}
    const size_type size(mytime t){
      size_type s=0;
      for(ppci pi=currentPatients.begin();pi!=currentPatients.end();pi++){
        if((*pi).second->admissionTime()<t) s++;
      }
      return s;
    }
    const size_type maxSize(){return capacity;}
    bool empty(){return currentPatients.empty();}
    bool full(){ return currentPatients.size()==maxSize();}
    bool has(int id){return currentPatients.find(id)!=currentPatients.end();}
    void add(int id, patient* p){
      if(full()) throw runtime_error("Ward already full!");
      assert(!has(id));
      ninfected_valid = false;
      currentPatients[id]=p;
    }
    void remove(int id){
      ninfected_valid = false;
      currentPatients.erase(id);
    } 
    int nInfected(mytime t){
      int n=0;
      for(ppmap::iterator pi=currentPatients.begin(); pi!= currentPatients.end(); pi++) 
        n += (*pi).second->isInfected(t);
      return n;
    }
    int nInfected(){
      //if(ninfected_valid) return ninfected;
      ninfected=0;
      for(ppmap::iterator pi=currentPatients.begin(); pi!= currentPatients.end(); pi++) 
        ninfected += (*pi).second->isInfected();
      ninfected_valid = true;
      return ninfected;
    }
    int nSusceptible(){
      return size()-nInfected();
    }
    int nSusceptible(mytime t){
      return size(t)-nInfected(t);
    }
    patient * anyByIndex(unsigned int i){
      if(empty()) throw runtime_error("Ward is empty.");
      ppmap::iterator it = byIndex(i);
      return (*it).second;
    }
    patient* removeByIndex(unsigned int i){
      if(empty()) throw runtime_error("Ward is empty.");
      ppmap::iterator it = byIndex(i);
      patient * pp = (*it).second;
      remove((*it).first);
      ninfected_valid = false;
      return pp;      
    }
    patient* uninfectedByIndex(unsigned int j){
      if(empty()) throw runtime_error("Ward is empty.");
      unsigned int i=j+1;
      ppmap::iterator it = currentPatients.begin(),
        end = currentPatients.end();
      while(i--){
        while(it!=end && (*it).second->isInfected())
          it++;
        if(it == end) 
          throw range_error("uninfected patient cannot be found.");
        //if(!((*(++it)).second->isInfected()) && it!=end)--i;
      }
      ninfected_valid = false;
      return (*it).second;
    }
    ppci begin(){
      return currentPatients.begin();
    }
    ppci end(){
      return currentPatients.end();
    }
};

class ward_v2{
  private:
    vector<pp> infected;
    vector<pp> susceptible;
    size_type capacity;
  public:
    ward_v2(unsigned int);
    bool empty();
    bool full();
    size_type size();
    size_type maxSize();
    size_type nInfected();
    size_type nSusceptible();
    void dischargeRandom(mytime);
    void infectRandom(mytime);	
    void add(patient*);
    void testSomeone(mytime,double,double);
};	
}

