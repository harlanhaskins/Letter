#ifndef LetterJIT_h
#define LetterJIT_h

// COFF.h redefines DEBUG within an enum, which breaks the build.
#pragma push_macro("DEBUG")
#undef DEBUG
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#pragma pop_macro("DEBUG")

#include "llvm/ExecutionEngine/RTDyldMemoryManager.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/LambdaResolver.h"
#include "llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h"
#include "llvm/IR/Mangler.h"
#include "llvm/Support/DynamicLibrary.h"

class LetterJIT {
public:
  typedef llvm::orc::ObjectLinkingLayer<> ObjLayerT;
  typedef llvm::orc::IRCompileLayer<ObjLayerT> CompileLayerT;
  typedef CompileLayerT::ModuleSetHandleT ModuleHandleT;

  LetterJIT()
      : targetMachine(EngineBuilder().selectTarget()), dataLayout(targetMachine->createDataLayout()),
        compileLayer(objectLayer, llvm::orc::SimpleCompiler(*targetMachine)) {
    llvm::sys::DynamicLibrary::LoadLibraryPermanently(nullptr);
  }

  TargetMachine &getTargetMachine() { return *targetMachine; }

  ModuleHandleT addModule(std::unique_ptr<Module> module) {

    auto resolver = llvm::orc::createLambdaResolver(
        [&](const std::string &name) {
          if (auto sym = findMangledSymbol(name))
            return RuntimeDyld::SymbolInfo(sym.getAddress(), sym.getFlags());
          return RuntimeDyld::SymbolInfo(nullptr);
        },
        [](const std::string &S) { return nullptr; });
    auto handle = compileLayer.addModuleSet(singletonSet(std::move(module)),
                                       make_unique<SectionMemoryManager>(),
                                       std::move(resolver));

    moduleHandles.push_back(handle);
    return handle;
  }

  void removeModule(ModuleHandleT H) {
    moduleHandles.erase(
        std::find(moduleHandles.begin(), moduleHandles.end(), H));
    compileLayer.removeModuleSet(H);
  }

  llvm::orc::JITSymbol findSymbol(const std::string name) {
    return findMangledSymbol(mangle(name));
  }

private:

  std::string mangle(const std::string &name) {
    std::string mangledName;
    {
      raw_string_ostream mangledNameStream(mangledName);
      Mangler::getNameWithPrefix(mangledNameStream, name, dataLayout);
    }
    return mangledName;
  }

  template <typename T> static std::vector<T> singletonSet(T t) {
    std::vector<T> vec;
    vec.push_back(std::move(t));
    return vec;
  }

    llvm::orc::JITSymbol findMangledSymbol(const std::string &name) {
    for (auto &handle : make_range(moduleHandles.rbegin(), moduleHandles.rend()))
      if (auto sym = compileLayer.findSymbolIn(handle, name, true))
        return sym;

    if (auto symAddr = RTDyldMemoryManager::getSymbolAddressInProcess(name))
      return llvm::orc::JITSymbol(symAddr, JITSymbolFlags::Exported);

    return nullptr;
  }

  std::unique_ptr<TargetMachine> targetMachine;
  const DataLayout dataLayout;
  ObjLayerT objectLayer;
  CompileLayerT compileLayer;
  std::vector<ModuleHandleT> moduleHandles;
};

#endif // LetterJIT_h
