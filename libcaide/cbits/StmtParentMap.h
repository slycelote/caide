//===--- ParentMap.h - Mappings from Stmts to their Parents -----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the ParentMap class.
//
//===----------------------------------------------------------------------===//

#pragma once

namespace clang {
class Stmt;
class Expr;
}

// A modification of clang::ParentMap class that tracks roots of the trees too
// (their parents are considered to be null pointers)
class StmtParentMap {
  void* Impl;
public:
  explicit StmtParentMap(clang::Stmt* ASTRoot);
  StmtParentMap(const StmtParentMap&) = delete;
  StmtParentMap(StmtParentMap&&) = delete;
  ~StmtParentMap();

  /// \brief Adds and/or updates the parent/child-relations of the complete
  /// stmt tree of S. All children of S including indirect descendants are
  /// visited and updated or inserted but not the parents of S.
  void addStmt(clang::Stmt* S);

  /// Manually sets the parent of \p S to \p Parent.
  ///
  /// If \p S is already in the map, this method will update the mapping.
  void setParent(const clang::Stmt *S, const clang::Stmt *Parent);

  clang::Stmt *getParent(clang::Stmt*) const;
  clang::Stmt *getParentIgnoreParens(clang::Stmt *) const;
  clang::Stmt *getParentIgnoreParenCasts(clang::Stmt *) const;
  clang::Stmt *getParentIgnoreParenImpCasts(clang::Stmt *) const;
  clang::Stmt *getOuterParenParent(clang::Stmt *) const;

  const clang::Stmt *getParent(const clang::Stmt* S) const {
    return getParent(const_cast<clang::Stmt*>(S));
  }

  const clang::Stmt *getParentIgnoreParens(const clang::Stmt *S) const {
    return getParentIgnoreParens(const_cast<clang::Stmt*>(S));
  }

  const clang::Stmt *getParentIgnoreParenCasts(const clang::Stmt *S) const {
    return getParentIgnoreParenCasts(const_cast<clang::Stmt*>(S));
  }

  bool contains(clang::Stmt* S) const;

  bool hasParent(clang::Stmt* S) const {
    return getParent(S) != nullptr;
  }

  bool isConsumedExpr(clang::Expr *E) const;

  bool isConsumedExpr(const clang::Expr *E) const {
    return isConsumedExpr(const_cast<clang::Expr*>(E));
  }
};

