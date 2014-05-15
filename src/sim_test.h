#pragma once
#ifndef __test_hpp
#define __test_hpp
#include "sim_event.h" // for mytime definition
namespace transsim{
/**
 *
 */
class test{
  private:
    mytime time;
    bool result;
  public:
    test(mytime t, bool r):time(t),result(r){}
    const bool isPositive()const{return result;}
    const mytime when()const{return time;}
    class byTestTime{
      private:
        bool r;
      public:
        byTestTime(const bool reverse=false):r(reverse){}
        bool operator()(const test& lhs, const test& rhs)const{
          if(r) return (lhs.time>rhs.time);
          else  return (lhs.time<rhs.time);
        }
        bool operator()(const test*const lhs, const test*const rhs)const{
          if(r) return ((*lhs).time>(*rhs).time);
          else  return ((*lhs).time<(*rhs).time);
        }
    };
};
}
#endif //__test_hpp
