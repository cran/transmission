
class History
{
protected:
        Event **h;
        int n;
        int cap;

        void ensure(int m)
        {
                if (m <= cap)
                        return;

                int newcap = cap;
                while (m > newcap)
                        newcap *= 2;

                Event **newh = new Event*[newcap];
                for (int i=0; i<n; i++)
                        newh[i] = h[i];

                delete h;
                h = newh;
                cap = newcap;
        }

public:
        History()
        {
                cap = 1000;
                h = new Event*[cap];
                n = 0;
        }

        History(int initcap) 
        {
                cap = initcap;
                h = new Event*[cap];
                n = 0;
        }

	~History()
	{
		if (h)
                	delete h;
	}

	void clear()
	{
		if (h)
			delete h;
		h = new Event*[cap];
		n = 0;
	}

        void reset(Event *e)
        {
		int j = e->index;

		for ( ; j>0 && e->t < h[j-1]->t; j--)
		{
			h[j] = h[j-1];
			h[j]->index = j;
			h[j]->y += e->dy;
			h[j]->z += e->dz;
			e->y -= h[j]->dy;
			e->z -= h[j]->dz;
		}
		e->index = j;
		h[j] = e;

		for ( ; j<n-1 && e->t > h[j+1]->t; j++)
		{
			h[j] = h[j+1];
			h[j]->index = j;
			h[j]->y -= e->dy;
			h[j]->z -= e->dz;
			e->y += h[j]->dy;
			e->z += h[j]->dz;
		}
		e->index = j;
		h[j] = e;
        }

        void insert(Event *e)
        {
                e->y = e->dy + ( n == 0 ? 0 : h[n-1]->y );
                e->z = e->dz + ( n == 0 ? 0 : h[n-1]->z );
                e->index = n;

                ensure(n+1);
                h[n++] = e;

                reset(e);
        }

	void append(Event *e)
	{
		ensure(n+1);
		h[n++] = e;
	}

        int length() const
        {
                return n;
        }

        Event *get(int i) const
        {
                if (i >= 0 && i < n)
                        return h[i];
                else 
                        return 0;
        }
};
