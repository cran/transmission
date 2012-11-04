
class InfectionMCMC
{
protected:
        UnitHistory *h;
        Calculator *c;
	Sampler *s;

	int dos;
	int dot;
	int doi;
	int dofp;
	int dofn;

	int maxs;
	int maxp;

	int its;

	double start;

public:
	InfectionMCMC()
	{
		start = clock();

		h = new UnitHistory();
		c = new Calculator(0.01,0.1,0,0.1);
		s = new Sampler(h,c); 

        dos = 1;
		dot = 1;
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

	void setParameters(double trans, double import, double falsepos, double falseneg)
	{
		c->setTransmission(trans);
		c->setImportation(import);
		c->setFalsePos(falsepos);
		c->setFalseNeg(falseneg);
	}
	
	void setTransmission(double trans)
	{
		c->setTransmission(trans);
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

	void setUpdateParameters(int t, int i, int fp, int fn)
	{
		dot = t;
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

	int nextSample(double *res)
	{
		if (!h->isFinalized())
			h->finalize();
        int ns = -1;
		if (dos)
			ns = s->sampleStates(maxs);
		if (doi)
			s->sampleImportation(maxp);
		if (dofn)
			s->sampleFalseNeg(maxp);
		if (dofp)
			s->sampleFalsePos(maxp);
		if (dot)
			s->sampleTransmission(maxp);

		res[0] = c->getTransmission();
		res[1] = c->getImportation();
		res[2] = c->getFalsePos();
		res[3] = c->getFalseNeg();

		its++;
        return ns;
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
