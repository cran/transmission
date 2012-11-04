#pragma once
#ifndef __event_hpp
#define __event_hpp
#include <limits>
#include <exception>
#include <assert.h>

using namespace std;
namespace transsim{
typedef double mytime;
extern const mytime time_unknown;

/*! Event class
 *
 * Wraps mytime
 */
class event{
  private:
    mytime time;
    event & operator=(const event&);
  public:
    event():time(time_unknown){}
    event(mytime t):time(t){}
    bool operator ==(const event& rhs) const{ return time == rhs.time; }
    bool operator <=(const event& rhs) const{ return time <= rhs.time; }
    bool operator >=(const event& rhs) const{ return time >= rhs.time; }
    bool operator <(const event& rhs) const{ return time < rhs.time; }
    bool operator >(const event& rhs) const{ return time > rhs.time; } 
    bool operator ==(const mytime lhs) const{ return time == lhs;}
    bool operator <=(const mytime lhs) const{ return time <= lhs;}
    bool operator >=(const mytime lhs) const{ return time >= lhs;}
    bool operator <(const mytime lhs) const{ return time < lhs;}
    bool operator >(const mytime lhs) const{ return time > lhs;}
    event& operator=(const mytime new_time){ time = new_time; return *this;}
    void clear(){time = time_unknown;}

    mytime when(){return time;}
    bool unknown()const{return time==time_unknown;}
    bool known()const{return !unknown();}
};

bool known(const mytime t);
bool unknown(const mytime t);
}
#endif //__event_hpp
