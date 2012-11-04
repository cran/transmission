
class Calculator 
{
protected:
        double lambda;
        double alpha;
        double falsepos;
        double falseneg;

	double loglambda;
	double logalpha;
	double logalphamin;
	double logfp;
	double logfpmin;
	double logfn;
	double logfnmin;

public:

        Calculator(double trans, double import, double falsep, double falsen)
        {
		setTransmission(trans);
		setImportation(import);
		setFalsePos(falsep);
		setFalseNeg(falsen);
        }

        double calc(int i, UnitHistory *h) 
        {
		Event *e = h->h[i];
                Event *p = h->h[i-1];

		int epat = e->pat;
		double etime = e->t;
		double py = p->y;

                double x = -lambda * py * (p->z - py) * (etime - p->t);

                switch (e->tp) 
                {
                case Event::infection:
                        if (h->a[epat]->t >= etime) 
                        {
                                x += logalpha;
                        } 
                        else 
                        {
                                x += logalphamin;

                                if (h->d[epat]->t > etime) 
                                {
                                        x += loglambda + log(py);
                                }
                        }
                        break;

                case Event::negtest:
			x += ( h->inf[epat]->t < etime ? logfn : logfpmin );
                        break;

                case Event::postest:
			x += ( h->inf[epat]->t > etime ? logfp : logfnmin );
                        break;
                }

                return x;
        }

        double calc(UnitHistory *h) 
        {
                double x = 0;
                for (int i = 1; i < h->length(); i++)
                        x += calc(i, h);
                return x;
        }

        double calc(UnitHistory *h, int after, int until) 
        {
                double x = 0;
                for (int i = after+1; i <=until; i++)
	                x += calc(i, h);
                return x;
        }

        void setTransmission(double t) 
        {
                lambda = t;
		loglambda = log(lambda);
        }

        double getTransmission() const
        {
                return lambda;
        }

        void setImportation(double a) 
        {
                alpha = a;
		logalpha = log(alpha);
		logalphamin = log(1-alpha);
        }

        double getImportation() const
        {
                return alpha;
        }

        void setFalsePos(double x) 
        {
                falsepos = x;
		logfp = log(falsepos);
		logfpmin = log(1-falsepos);
        }

        double getFalsePos() const
        {
                return falsepos;
        }

        void setFalseNeg(double x) 
        {
                falseneg = x;
		logfn = log(falseneg);
		logfnmin = log(1-falseneg);
        }

        double getFalseNeg() const
        {
                return falseneg;
        }

        friend ostream& operator <<(ostream& out, Calculator *c); 
};

ostream& operator <<(ostream& os, Calculator *c)
{
       return os << "\t" << c->getTransmission() 
		<< "\t" << c->getImportation() 
		<< "\t" << c->getFalseNeg() 
		<< "\t" << c->getFalsePos() ;
}
