#pragma once
namespace transfitting{
class ParameterSet{
  public:  // yes public members
    double transmission, importation, sensitivity;
    static ParameterSet& Instance(){
      static ParameterSet _instance;
      return _instance;
    }
  private:
    //singleton enforced
    ParameterSet():transmission(0), importation(0), sensitivity(0){};
    ParameterSet(ParameterSet&);
    ParameterSet& operator=(ParameterSet const&);
};
}

