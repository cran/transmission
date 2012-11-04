
class UnitHistory : public History
{
friend class Calculator;
friend class Sampler;

protected:
	int npat;
	int *gotpat;
	Event **a;		//!< Admissions indexed by patient id
	Event **d;		//!< Discharges indexed by patient id
	Event **inf;      	//!< infections indexed by patient id
	int *ns;		 //!< Number of tests per patient
	Event ***s;      	 //!< List by patient id of tests for each patient.
	double maxtime;	  	//!< maximum time of events in history.
	double mintime;	  	//!< minimum time in history
	int finalized;

	// Error handling.
	string errout;
	int wantwarnings;
	int fatal;

	void error(Event *e, string message)
	{
		if (!fatal)
			errout = "Errors in input data:\n\n";

		fatal = 1;

		stringstream s (stringstream::out);
		s << "Event :\t" << e << "\n\t";
		s << message << "\n\n";
	
		errout = errout+s.str();
	}

	void error(int pat, string message)
	{
		if (!fatal)
			errout = "Errors in input data:\n\n";

		fatal = 1;

		stringstream s (stringstream::out);
		s << "Patient :\t" << pat << "\n\t";
		s << message << "\n\n";
	
		errout = errout+s.str();
	}

	void reportErrors()
	{
		if (fatal)
			throw(new logic_error(errout.c_str()));
	}

	void warning(Event *e, string message)
	{
	}
		
	void reCheckErrors()
	{
		for (int i=0; i<npat; i++)
		{
			if (!gotpat[i])
				continue;
			
			Event *a = getAdmission(i);
			Event *f = getInfection(i);
			Event *d = getDischarge(i);
			int nt = nTests(i);
			Event **t = getTests(i);

			if (a == 0)
				error(i,"Admission event is missing.");
			if (d == 0)
				error(i,"Discharge event is missing.");
			if (f == 0)
				error(i,"Infection event is missing.");

			if (a->time() > d->time())
				error(a,"Admission time is after discharge time.");
			if (f->time() < a->time())
				error(f,"Infection time is before admission time.");
			if (f->time() > d->time())
				error(f,"Infection time is after discharge time.");

			if (a->getIndex() > d->getIndex())
				error(a,"Admission index is greater than discharge index.");
			if (f->getIndex() > d->getIndex())
				error(f,"Infection index is greater than discharge index.");
			if (f->getIndex() < a->getIndex())
				error(f,"Infection index is less than admission index.");

			for (int j=0; j<nt; j++)
			{
				if (t[j]->time() < a->time())
					error(t[j],"Test time is before admission time.");
				if (t[j]->time() > d->time())
					error(t[j],"Test time is after discharge time.");
				if (t[j]->getIndex() < a->getIndex())
					error(t[j],"Test index is less than admision index.");
				if (t[j]->getIndex() > d->getIndex())
					error(t[j],"Test index is greater than discharge index.");
			}
		}

		for (int i=0; i<length(); i++)
		{
			if (h[i]->time() < mintime)
				error(h[i],"Event time is before minimum time.");
			if (h[i]->time() > maxtime)
				error(h[i],"Event time is after maximum time.");
		}
	}

	int nPosTests(int i)
	{
		if (!gotPatient(i))
			return 0;

		int x = 0;
		for (int j=0; j<ns[i]; j++)
			if (s[i][j]->type() == Event::postest)
				x++;
		return x;
	}

public:
	UnitHistory(): History()
	{
		a = 0;
		d = 0;
		inf = 0;
		ns = 0;
		s = 0;
		gotpat = 0;

		npat = 0;
		mintime = 100000000;
		maxtime = -100000000;
		finalized = 0;
		wantwarnings = 0;
		fatal = 0;
	}

	~UnitHistory()
	{
		if(a)
			delete a;
		if(d)
			delete d;
		if(inf)
			delete inf;
		if (ns)
			delete ns;
		
		if(s)
		{
			for (int i=0; i<npat; i++)
				if (s[i])
					delete s[i];
			delete s;
		}

		if (gotpat)
			delete gotpat;

		if (h)
		{
			for (int i=0; i<length(); i++)
				delete h[i];
			delete h;
			h = 0;
		}
	}

	int isFinalized()
	{
		return finalized;
	}

	void add(double time, int patient, int type)
	{
		Event *e = new Event(time,patient,type);

		if (finalized)
			error(e,"Can't add event to finalized history.");

		append(e);

		if (npat < e->patient())
			npat = e->patient();
		if (maxtime < e->time())
			maxtime = e->time();
		if (mintime > e->time())
			mintime = e->time(); 
	}

	void finalize()
	{	
		maxtime += 0.001;
		npat++;

		// Allocate and initialize arrays.

		a = new Event*[npat];
		d = new Event*[npat];
		inf = new Event*[npat];
		ns = new int[npat];
		s = new Event**[npat];
		gotpat = new int[npat];

		for (int i=0; i<npat; i++)
		{
			a[i] = 0;
			d[i] = 0;
			ns[i] = 0;
			gotpat[i] = 0;
			inf[i] = 0;
			s[i] = 0;
		}

		// Mark valid patient ids.
		// Assign admissions and discharges. 
		// Count tests.

		for (int i=0; i<length(); i++)
		{
			Event *e = h[i];
			
			if (e->patient() >= 0)
				gotpat[e->patient()] = 1;

			switch(e->type())
			{
			case Event::admission:
				if (a[e->patient()] != 0)
					error(e,"Multiple admissions for patient.");
				a[e->patient()] = e;
				break;

			case Event::discharge:
				if (d[e->patient()] != 0)
					error(e,"Multpile discharges for patient.");
				d[e->patient()] = e;
				break;

			case Event::negtest:
			case Event::postest:
				ns[e->patient()]++;
				break;
			}
		}

		// Fill in missing admissions and discharges.
		// Allocate test arrays.

		for (int i=0; i<npat; i++)
		{
			if (!gotpat[i])
				continue;

			if (a[i] == 0)
			{
				a[i] = new Event(mintime,i,Event::admission);
				append(a[i]);
				warning(a[i],"Adding addmission time for patient.");
			}
			if (d[i] == 0)
			{
				d[i] = new Event(maxtime,i,Event::discharge);
				append(d[i]);
				warning(d[i],"Adding discharge time for patient.");
			}

			s[i] = new Event*[ns[i]];
			ns[i] = 0;
		}

		// Assign tests.
		// Assign observed infections.

		for (int i=0; i<length(); i++)
		{
			Event *e = h[i];
			switch(e->type())
			{
			case Event::negtest:
			case Event::postest:
				s[e->patient()][ns[e->patient()]++] = e;
				break;

			case Event::infection:
				int pat = e->patient();

				if (e->time() < a[pat]->time())
					error(e,"Infection time is before admission time.");
				if (e->time() > d[pat]->time())
					error(e,"Infection time after discharge time.");

				inf[pat] = e;
				break;
			}
		}

		// Assign unobserved infections.

		for (int i=0; i<npat; i++)
		{
			if (!gotpat[i])
				continue;

			if (inf[i] == 0)
			{
				inf[i] = new Event( (nPosTests(i) ? a[i]->time() : d[i]->time() ) ,i,Event::infection);
				append(inf[i]);
			}
		}

		// Clear history and remake it by adding the events
		// in an order that ensures consistency.

		clear();

		insert(new Event(mintime,-1,Event::marker));

		for (int i=0; i<npat; i++)
		{
			if (!gotpat[i])
				continue;

			insert(a[i]);
			insert(inf[i]);
			for (int j=0; j<ns[i]; j++)
				insert(s[i][j]);
			insert(d[i]);
		}

		// Double check for errors.
		// Throw exception if there are any remaining errors.

		reCheckErrors();
		reportErrors();
		finalized = 1;
	}

	int nPatients() const
	{
		return npat;
	}

	int gotPatient(const int i) const
	{
		return i >=0 && i < npat && gotpat[i];
	}

	Event *getInfection(const int i) const
	{
		if (!gotPatient(i))
			return 0;

		return inf[i];
	}

	Event *getAdmission(const int i) const
	{
		if (!gotPatient(i))
			return 0;

		return a[i];
	}

	Event *getDischarge(const int i) const
	{
		if (!gotPatient(i))
			return 0;

		return d[i];
	}

	Event **getTests(const int i) const
	{
		if (!gotPatient(i))
			return 0;

		return s[i];
	}

	int nTests(int i) const
	{
		if (!gotPatient(i))
			return 0;

		return ns[i];
	}

	int isNewInfection(Event *e)
	{
		return e->type() == Event::infection 
				&& e->time() > getAdmission(e->patient())->time()
				&& e->time() < getDischarge(e->patient())->time() ;
	}

	friend ostream& operator <<(ostream& os, UnitHistory *h);

	friend istream& operator >>(istream& is, UnitHistory *h);
};

ostream& operator <<(ostream& os, UnitHistory *h)
{
	for (int i=1; i<h->length(); i++)
		os << h->h[i] << "\n";
	return os;
}

istream& operator >>(istream& is, UnitHistory *h)
{
	char yyy = 0;

	while(true)
	{
		double time = 0;
		int patient = 0;
		int type = 0;

		if (!( (is >> time) && (is >> patient) && (is >> type) ))
			break;

		do
		{
			is.get(yyy);
		}
		while ( yyy != '\n');

		h->add(time,patient,type);
	}

	h->finalize();

	return is;
}
