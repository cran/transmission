
double defaultrand()
{
	return rand() / (double) RAND_MAX;
}

class Sampler
{
private:
protected:
        UnitHistory *h;
        Calculator *c;

	double (*myrand)();

        double runif()
        {
		return (*myrand)();
        }

        double rexp()
        {
            double z;
            do{
                z = -log(runif());
            }while(isinf(z));
		return z;
        }

       double rnorm()
        {
            double u;
            double v;
            double z;
            do{
                u = runif();
                v = runif();
                z = sqrt(-2*log(u))*cos(2*acos(-1)*v);
            }while(u==0 || u==1);
		return z;
        }

public:
        Sampler(UnitHistory *hist, Calculator *calc)
	{
		h = hist;
		c = calc;
		myrand = defaultrand;
	}

        Sampler(UnitHistory *hist, Calculator *calc, double (*randf)())
        {
		h = hist;
		c = calc;
		myrand = randf;
        }

	void setRandomUniform(double (*randf)())
	{
		myrand = randf;
	}

        int sampleStates(int max)
        {
		int naccept = 0;
                for (int i = 0; i < h->nPatients(); i++)
			if (h->gotPatient(i))
                        	naccept += sampleState(i,max);

		return naccept;
        }

        int sampleState(int i, int max)
        {
                Event *e = h->inf[i];
                double a = h->a[i]->t;
                double d = h->d[i]->t;

                double oldtime = e->t;
                double newtime = 0;

                double U = runif();
                double prob = 1.0 / (2.0 + (d - a));

                if (U < prob)
                {
                        newtime = a;
                }
                else if (U < 2*prob)
                {
                        newtime = d;
                }
                else
                {
                        newtime = a + runif() * (d - a);
                }

		double lo = newtime;
		if (lo > oldtime)
			lo = oldtime;

		double hi = newtime;
		if (hi < oldtime)
			hi = oldtime;

		int ia;
		for (ia = e->index-1; h->h[ia]->t >= lo && ia > 0; ia--)
            ;

		int id;
		for (id = e->index+1; h->h[id]->t <= hi && id < h->length()-1 ; id++)
            ;

                double accept = -c->calc(h,ia,id);

                e->setTime(newtime);
                h->reset(e);

                accept += c->calc(h, ia, id);

                if ( ( max ? 0 : -rexp() ) <= accept)
		{
                        return 1;
		}

                e->setTime(oldtime);
                h->reset(e);
                return 0;
        }

        void sampleImportation(int max)
        {
                double x = ( max ? 0 : rexp() );
                double y = ( max ? 0 : rexp() );

                for (int i = 0; i < h->nPatients(); i++)
                {
			if (!h->gotPatient(i))
				continue;
                        if (h->getInfection(i)->time() > h->getAdmission(i)->time())
                                y += ( max ? 1 : rexp() );
                        else
        	                x += ( max ? 1 : rexp() );
                }
                c->setImportation(x/(x+y));
        }

        void sampleFalseNeg(int max)
        {
                double x = ( max ? 0 : rexp() );
                double y = ( max ? 0 : rexp() );

                for (int i = 0; i < h->length(); i++)
                {
                        Event *e = h->get(i);

                        switch (e->type())
                        {
                        case Event::postest:
                                if (e->time() > h->getInfection(e->patient())->time())
                                        y += ( max ? 1 : rexp() );
                                break;
        
                        case Event::negtest:
                                if (e->time() > h->getInfection(e->patient())->time())
                                        x += ( max ? 1 : rexp() );
                                break;
                        }
                }
        
                c->setFalseNeg(x/(x+y));
        }

        void sampleFalsePos(int max)
        {
                double x = ( max ? 0 : rexp() );
                double y = ( max ? 0 : rexp() );

                for (int i = 0; i < h->length(); i++)
                {
                        Event *e = h->get(i);

                        switch (e->type())
                        {
                        case Event::postest:
                                if (e->time() < h->getInfection(e->patient())->time())
                                        y += ( max ? 1 : rexp() );
                                break;
        
                        case Event::negtest:
                                if (e->time() < h->getInfection(e->patient())->time())
                                        x += ( max ? 1 : rexp() );
                                break;
                        }
                }

                c->setFalsePos(y/(x+y));
        }

        void sampleSlope(int max)
        {
            if (!(c->using_slope)) throw(runtime_error("cannot sample slope."));
		    double SlopeSD = c->getSlopeSD();
		    double lambda = c->getSlope();
            double oldl = c->calc(h);

            double newlambda = lambda + SlopeSD*rnorm();
            c->setSlope(newlambda);
            double newl = c->calc(h);
            double x = ( max ? 0 : -rexp() ); // Is this right?  Should it be max or min or what...???
//            cerr << lambda << "\t" << newlambda << "\t" << oldl << "\t" << newl << '\t' << newl-oldl << "\t" << x << endl;
            if (x > newl - oldl)
            {
                c->setSlope(lambda);
            }
        }

        void sampleTransmission(int max) 
        {
            if(c->using_slope) {
		        double IntSD = c->getTransmissionSD();
		        double lambda0 = c->getTransmission();
                double loglambda0 = log(lambda0);
                double oldl = c->calc(h);

                double newloglambda0 = loglambda0 + IntSD*rnorm(); // need to define this function/operation!!!
                double newlambda0 = exp(newloglambda0); // need to define this function/operation!!!
                c->setTransmission(newlambda0);
                double newl = c->calc(h);
                double x = ( max ? 0 : -rexp() ); // Is this right?  Should it be max or min or what...???
    //            cerr << lambda0 << "\t" << newlambda0 << "\t" << "\t" << newl - oldl - newloglambda0 + loglambda0 << '\t' << x << endl;
                if (x > newl - oldl - newloglambda0 + loglambda0)
                {
                    c->setTransmission(lambda0);
                }
            } else {
		        double rate = 0;
		        double x = ( max ? 0 : rexp() );

		        Event* p = 0;
		        Event* e = 0;

		        for (int i=1; i<h->length(); i++)
		        {
			        e = h->get(i);
			        p = h->get(i-1);

			        rate += p->yValue() * (p->zValue() - p->yValue()) * (e->time() - p->time());

			        if (h->isNewInfection(e))
				        x += ( max ? 1 : rexp() );
		        }

		        if (x > 0 && rate > 0)
			        c->setTransmission(x/rate);
            }
        }
};
