from __future__ import print_function, absolute_import, division
import _Fractions
import f90wrap.runtime
import logging

class Fractions(f90wrap.runtime.FortranModule):
    """
    Module fractions
    
    
    Defined at fractions.f90 lines 1-29
    
    """
    @f90wrap.runtime.register_class("Fractions.Fraction")
    class Fraction(f90wrap.runtime.FortranDerivedType):
        """
        Type(name=fraction)
        
        
        Defined at fractions.f90 lines 3-5
        
        """
        def __init__(self, handle=None):
            """
            self = Fraction()
            
            
            Defined at fractions.f90 lines 3-5
            
            
            Returns
            -------
            this : Fraction
            	Object to be constructed
            
            
            Automatically generated constructor for fraction
            """
            f90wrap.runtime.FortranDerivedType.__init__(self)
            result = _Fractions.f90wrap_fraction_initialise()
            self._handle = result[0] if isinstance(result, tuple) else result
        
        def __del__(self):
            """
            Destructor for class Fraction
            
            
            Defined at fractions.f90 lines 3-5
            
            Parameters
            ----------
            this : Fraction
            	Object to be destructed
            
            
            Automatically generated destructor for fraction
            """
            if self._alloc:
                _Fractions.f90wrap_fraction_finalise(this=self._handle)
        
        _dt_array_initialisers = []
        
    
    @staticmethod
    def init(num, denom):
        """
        init = init(num, denom)
        
        
        Defined at fractions.f90 lines 8-16
        
        Parameters
        ----------
        num : int
        denom : int
        
        Returns
        -------
        init : Fraction
        
        """
        init = _Fractions.f90wrap_init(num=num, denom=denom)
        init = f90wrap.runtime.lookup_class("Fractions.Fraction").from_handle(init)
        return init
    
    @staticmethod
    def adder(self, f):
        """
        adder = adder(self, f)
        
        
        Defined at fractions.f90 lines 19-29
        
        Parameters
        ----------
        self : Fraction
        f : Fraction
        
        Returns
        -------
        adder : Fraction
        
        """
        adder = _Fractions.f90wrap_adder(self=self._handle, f=f._handle)
        adder = f90wrap.runtime.lookup_class("Fractions.Fraction").from_handle(adder)
        return adder
    
    _dt_array_initialisers = []
    

fractions = Fractions()

