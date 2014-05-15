
class Event
{
friend class History; // friends can use classes proteccted information.
friend class Calculator;
friend class Sampler;

protected: // member of same class or member of derived class can use protected
        int index; // create needed variables
        double t;
        double y;
        double dy;
        double z;
        double dz;
	int pat;
	int tp;

public: // available for all to use
	enum // relating event-types with integer values
    {
        marker    = -1,
        admission = 0,
        negtest   = 1,
        postest   = 2,
        discharge = 3,
        infection = 4
};

	Event(double time, int patient, int kind) // Event function within Event class
	{
		pat = patient;
		t = time;
		tp = kind;
		dy = 0;
		dz = 0;
		index = 0;

		switch(kind) // if kind = admission make dy = 0, dz = 1; etc...
		{
		case admission:
			dy = 0;
			dz = 1;
			break;

		case discharge:
			dy = -1;
			dz = -1;
			break;

		case infection:
			dy = 1;
			dz = 0;
			break;

		case marker: // Hmm... not sure what marker represents yet
			pat = -1;
			break;
		}
	}

        ~Event() // defines the desctructor of Event()
        {
        }

        double time() const
        {
                return t;
        }

        void setTime(const double time)
        {
                t = time;
        }

        double yValue() const
        {
                return y;
        }

        double zValue() const
        {
                return z;
        }

        int getIndex() const
        {
                return index;
        }

	int patient() const
	{
		return pat;
	}

	int type() const
	{
		return tp;
	}

	friend ostream& operator <<(ostream& out, Event *e);
};

ostream& operator <<(ostream& os, Event *e)
{
	return os << e->t << "\t" << e->pat << "\t" << e->tp;
}
