
#ifndef _INFECTION_DEFINED

	#define _INFECTION_DEFINED
	
	#include <stdlib.h>
	#include <math.h>
	#include <string.h>
	#include <time.h>

	#include <iostream>
	#include <sstream>
	#include <exception>
	#include <stdexcept>

	using namespace std;

	namespace infection
	{
		#include "Event.h"
		#include "History.h"
		#include "UnitHistory.h"
		#include "Calculator.h"
		#include "Sampler.h"
		#include "InfectionMCMC.h"
	}

#endif
