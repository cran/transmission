
class Calculator
{
protected:
        double lambda;           //!< Constant transmission or transmission intercept
        double lambda_slope;     //!< transmission slope
        double alpha;            //!< importation probability
        double falsepos;         //!< False positive rate
        double falseneg;         //!< Flage negative rate 
        double IntSD;            //!< Intercept standard deviation
        double SlopeSD;          //!< Slope standard deviation

	double loglambda;
	double logalpha;
	double logalphamin;
	double logfp;
	double logfpmin;
	double logfn;
	double logfnmin;

public:
        const bool using_slope;  //!< Are we fitting with the slop or not?

        Calculator(): using_slope(false)
        {
            setTransmission(0);
            setImportation(0);
            setFalsePos(0);
            setFalseNeg(0);
            setTransmissionSD(1);
            setSlopeSD(1);
        }
        Calculator(bool use_slope): using_slope(use_slope)
        {
            setTransmission(0);
            setImportation(0);
            setFalsePos(0);
            setFalseNeg(0);
            setTransmissionSD(1);
            setSlopeSD(1);
        }
        Calculator(double trans, double import, double falsep, double falsen)
        : using_slope(false)
        {
            setTransmission(trans);
            setImportation(import);
            setFalsePos(falsep);
            setFalseNeg(falsen);
            setTransmissionSD(1);
            setSlopeSD(1);
        }

        Calculator(double trans, double trans1, double import, double falsep, double falsen)
        : using_slope(true)
        {
            setTransmission(trans);
            setSlope(trans1);
            setImportation(import);
            setFalsePos(falsep);
            setFalseNeg(falsen);
            setTransmissionSD(1);
            setSlopeSD(1);
        }

        double calc(int i, UnitHistory *h)
        {
		Event *e = h->h[i];
                Event *p = h->h[i-1];

		int epat = e->pat;
		double etime = e->t;
		double otime = p->t;
		double py = p->y;

                double x = ((using_slope)
                         ? py * (p->z - py) *(-lambda*(etime - otime) - lambda_slope*(etime*etime - otime*otime)/2)
                         : py * (p->z - py) *(-lambda) * (etime - p->t)
                         );

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
                                        x += (using_slope)
                                           ? (log(py) + (log(lambda + lambda_slope*etime)))
                                           : (log(py) + (loglambda));
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
                {
                        x += calc(i, h);
                }
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
            lambda    = t;
            loglambda = log(lambda);
        }

        double getTransmission() const
        {
                return lambda;
        }

        void setSlope(double t0)
        {
                lambda_slope = t0;
        }

        double getSlope() const
        {
                return lambda_slope;
        }

        void setSlopeSD(double tSD)
        {
                SlopeSD = tSD;
        }

        double getSlopeSD() const
        {
                return SlopeSD;
        }

        void setTransmissionSD(double t0SD)
        {
                IntSD = t0SD;
        }

        double getTransmissionSD() const
        {
                return IntSD;
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
        os << "\t" << c->getTransmission();
        if(c->using_slope)
            os << "\t" << c->getSlope();
        os << "\t" << c->getImportation();
        os << "\t" << c->getFalseNeg();
        os << "\t" << c->getFalsePos() ;
        return os;
}
