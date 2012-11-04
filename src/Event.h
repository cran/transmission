
class Event 
{
friend class History;
friend class Calculator;
friend class Sampler;

protected:
        int index;
        double t;
        double y;
        double dy;
        double z;
        double dz;
	int pat;
	int tp;

public:
    enum{
          marker    = -1,
          admission = 0,
          negtest   = 1,
          postest   = 2,
          discharge = 3,
          infection = 4};
	Event(double time, int patient, int kind)
	{
		pat = patient;
		t = time;
		tp = kind;
		dy = 0;
		dz = 0;
		index = 0;

		switch(kind)
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

		case marker:
			pat = -1;
			break;
		}
	}

        ~Event()
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
