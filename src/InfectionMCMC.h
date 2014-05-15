
class InfectionMCMC
{
protected:
    UnitHistory *h;
    Calculator *c;
	Sampler *s;

	bool dos;
	bool dot;
    bool dot1;
	bool doi;
	bool dofp;
	bool dofn;

	int maxs;
	int maxp;

	int its;

	double start;

public:
	InfectionMCMC()
	{
		start = clock();

		h = new UnitHistory();
		c = new Calculator();
		s = new Sampler(h,c);

        dos = 1;
		dot = 1;
        dot1 = 1;
		doi = 1;
		dofp = 0;
		dofn = 1;

		maxs = 0;
		maxp = 0;

		its = 0;
	}
	InfectionMCMC(bool use_slope)
	{
		start = clock();

		h = new UnitHistory();
		c = new Calculator(use_slope);
		s = new Sampler(h,c);

        dos = 1;
		dot = 1;
        dot1 = 1;
		doi = 1;
		dofp = 0;
		dofn = 1;

		maxs = 0;
		maxp = 0;

		its = 0;
	}

	~InfectionMCMC()
	{
		delete s;
		delete c;
		delete h;
	}

	void addEvent(double time, int patient, int type)
	{
		h->add(time,patient,type);
	}

	void setRandomUniform(double (*randf)())
	{
		s->setRandomUniform(randf);
	}

//	void setParameters(double trans, double import, double falsepos, double falseneg)
//	{
//		c->setTransmission(trans);
//		c->setImportation(import);
//		c->setFalsePos(falsepos);
//		c->setFalseNeg(falseneg);
//	}

	void setTransmission(double trans)
	{
		c->setTransmission(trans);
	}

	void setSlope(double trans0)
	{
		c->setSlope(trans0);
	}

	void setTransmissionSD(double transSD)
	{
		c->setTransmissionSD(transSD);
	}

	void setSlopeSD(double trans0SD)
	{
		c->setSlopeSD(trans0SD);
	}

	void setImportation(double import)
	{
		c->setImportation(import);
	}

	void setFalsePos(double falsepos)
	{
		c->setFalsePos(falsepos);
	}

	void setFalseNeg(double falseneg)
	{
		c->setFalseNeg(falseneg);
	}

	double getTransmission()
	{
		return c->getTransmission();
	}

    double getSlope()
    {
        return c->getSlope();
    }

	double getTransmissionSD()
    {
        return c->getTransmissionSD();
    }

    double getSlopeSD()
    {
        return c->getSlopeSD();
    }


	double getImportation()
	{
		return c->getImportation();
	}

	double getFalsePos()
	{
		return c->getFalsePos();
	}

	double getFalseNeg()
	{
		return c->getFalseNeg();
	}


	void setUpdateStates(int s)
	{
		dos = s;
	}

	void setUpdateParameters(int t, int t1, int i, int fp, int fn)
	{
		dot = t;
        dot1 = t1;
		doi = i;
		dofp = fp;
		dofn = fn;
	}

	void setMaximizeStates(int mxs)
	{
		maxs = mxs;
	}

	void setMaximizeParameters(int mxp)
	{
		maxp = mxp;
	}

	void nextSample(double *res = NULL)
	{
		if (!h->isFinalized())
			h->finalize();

		if (dos)
			s->sampleStates(maxs);
		if (doi)
			s->sampleImportation(maxp);
		if (dofn)
			s->sampleFalseNeg(maxp);
		if (dofp)
			s->sampleFalsePos(maxp);
		if (dot)
			s->sampleTransmission(maxp);
        if (dot1 && c->using_slope)
            s->sampleSlope(maxp);
        if(res){
		    res[0] = c->getTransmission();
		    res[1] = c->getImportation();
		    res[2] = c->getFalsePos();
		    res[3] = c->getFalseNeg();
        }
		its++;
	}

	int nSamples()
	{
		return its;
	}

	double getLikelihood()
	{
		return c->calc(h);
	}

	double timeTaken()
	{
		return (clock() - start) / CLOCKS_PER_SEC;
	}
};
